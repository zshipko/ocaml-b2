(* TODO:
 * - list_unfinished_large_files *)

open Lwt
open B2_client

let endpoint_base = ref "https://api.backblazeb2.com/b2api/"

module V1 = struct

    let endpoint path =  !endpoint_base ^ "v1" ^ "/" ^ path

    type token = {
        accountId: string;
        authorizationToken: string;
        apiUrl: string;
        downloadUrl: string;
        minimumPartSize: int;
    }

    (* Makes a new header object with the given token *)
    let make_header ?fields:(fields=[]) token =
        Cohttp.Header.of_list ([
            "Authorization", token.authorizationToken
        ] @ fields)

    type file = {
        fileId : string;
        accountId : string;
        bucketId : string;
        fileName : string;
    }

    type file_version = {
        fileId: string;
        fileName: string;
    }

    type file_info = {
        fileId: string;
        fileName: string;
        accountId: string;
        contentSha1: Cstruct.t;
        bucketId: string;
        contentLength: int64;
        contentType: string;
        fileInfo: Ezjsonm.value;
        action: string;
        uploadTimestamp: Int64.t;
    }

    type partial_file_info = {
        fileId: string;
        fileName: string;
        accountId: string;
        bucketId: string;
        contentType: string;
        fileInfo: Ezjsonm.value;
        uploadTimestamp: Int64.t;
    }

    let authorize_account (accountId: string) (applicationKey : string) : token Lwt.t =
        let encoded = Nocrypto.Base64.encode (Cstruct.of_string (accountId ^ ":" ^ applicationKey)) |> Cstruct.to_string in
        let headers = Cohttp.Header.of_list [
            "Authorization", "Basic " ^ encoded
        ] in
        B2_client.get_json ~headers (endpoint "authorize_account")
        >|= handle_error (fun j -> {
            accountId = account_id j;
            authorizationToken = find_string j ["authorizationToken"];
            apiUrl= find_string j ["apiUrl"];
            downloadUrl = find_string j ["downloadUrl"];
            minimumPartSize = find_int j ["minimumPartSize"] |> Int64.to_int;
        })

    (* Files *)

    let delete_file_version token (fileName : string) (fileId : string) : file_version Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O [
            "fileName", `String fileName;
            "fileId", `String fileId;
        ]) (endpoint "delete_file_version")
        >|= handle_error (fun j -> {
            fileId = file_id j;
            fileName = file_name j;
        })

   type download_authorization = {
        bucketId: string;
        fileNamePrefix: string;
        authorizationToken: string;
    }

    let get_download_authorization token bucketId fileNamePrefix validDurationInSeconds : download_authorization Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O [
            "bucketId", `String bucketId;
            "fileNamePrefix", `String fileNamePrefix;
            "validDurationInSeconds", `String (string_of_int validDurationInSeconds);
        ]) (endpoint "get_download_authorization")
        >|= handle_error (fun j -> {
            bucketId = bucket_id j;
            fileNamePrefix = find_string j ["fileNamePrefix"];
            authorizationToken = find_string j ["authorizationToken"];
        })

    let download_file_by_id ?token ?url:(url="") (fileId: string) : string Lwt.t =
        let headers, url = match token with
        | Some tok ->
            Some (make_header tok), tok.downloadUrl
        | None -> None, url in
        B2_client.get ?headers (url ^ "/download_file_by_id?fileId=" ^ fileId)

    let download_file_by_name ?token ?auth ?url:(url="") (fileName: string) : string Lwt.t =
        let headers, url = match token with
        | Some tok ->
            Some (make_header tok), tok.downloadUrl
        | None -> (match auth with
        | Some a -> Some (Cohttp.Header.of_list [
            "Authorization", a.authorizationToken
        ])
        | None -> None), url in
        B2_client.get ?headers (url ^ "/download_file_by_name?fileName=" ^ fileName)

    let find_all_file_info j : file_info = {
        fileId = file_id j;
        fileName = file_name j;
        accountId = account_id j;
        contentSha1 = find_cstruct j ["contentSha1"];
        bucketId = bucket_id j;
        contentLength = find_int j ["contentLength"];
        contentType = find_string j ["contentType"];
        fileInfo = Ezjsonm.find j ["fileInfo"];
        action = find_string j ["action"];
        uploadTimestamp = find_int j ["uploadTimestamp"];
    }

    let find_all_partial_file_info j : partial_file_info = {
        fileId = file_id j;
        fileName = file_name j;
        accountId = account_id j;
        bucketId = bucket_id j;
        contentType = find_string j ["contentType"];
        fileInfo = Ezjsonm.find j ["fileInfo"];
        uploadTimestamp = find_int j ["uploadTimestamp"];
    }

    let get_file_info token fileId : file_info Lwt.t =
        B2_client.post_json ~headers:(make_header token) ~json:(`O [
            "fileId", `String fileId;
        ]) (endpoint "get_file_info")
        >|= handle_error find_all_file_info

    let hide_file token bucketId fileName : file_info Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O [
            "bucketId", `String bucketId;
            "fileName", `String fileName;
        ]) (endpoint "hide_file")
        >|= handle_error find_all_file_info

    let list_file_names token ?startFileName:(startFileName="")
                              ?maxFileCount:(maxFileCount=0)
                              ?prefix:(prefix="")
                              ?delimiter bucketId : (file_info list * string) Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O ([
            "bucketId", `String bucketId;
            "maxFileCount", `Float (float_of_int maxFileCount);
            "prefix", `String prefix;
            "delimiter", (match delimiter with
            | Some d -> `String d
            | None -> `Null);
        ] @ (if startFileName <> "" then [
            "startFileName", `String startFileName;
        ] else []))) (endpoint "list_file_names")
        >|= handle_error (fun j ->
            Ezjsonm.find j ["files"]
            |> Ezjsonm.get_list find_all_file_info
            |> fun l -> l, find_string j ["nextFileName"])

    let list_file_versions token ?startFileName:(startFileName="")
                                 ?startFileId:(startFileId="")
                                 ?maxFileCount:(maxFileCount=0)
                                 ?prefix:(prefix="")
                                 ?delimiter bucketId : (file_info list * string * string) Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O ([
            "bucketId", `String bucketId;
            "maxFileCount", `Float (float_of_int maxFileCount);
            "prefix", `String prefix;
            "delimiter", (match delimiter with
            | Some d -> `String d
            | None -> `Null);
        ] @ (if startFileName <> "" && startFileId <> "" then [
            "startFileName", `String startFileName;
            "startFileId", `String startFileId
        ] else []))) (endpoint "list_file_versions")
        >|= handle_error (fun j ->
            Ezjsonm.find j ["files"]
            |> Ezjsonm.get_list find_all_file_info
            |> fun l -> l, find_string j ["nextFileName"], find_string j ["nextFileId"])

    type upload_url = {
        uploadUrl: string;
        authorizationToken: string;
    }

    let hash_string s =
        let h = Nocrypto.Hash.SHA1.init () in
        let _ = Nocrypto.Hash.SHA1.feed h (Cstruct.of_string s) in
        Nocrypto.Hash.SHA1.get h |> Cstruct.to_string

    let upload_file (url : upload_url) ?contentType:(contentType="b2/x-auto") ?fileInfo:(fileInfo=[]) (data : char Lwt_stream.t) fileName  : file_info Lwt.t =

        Lwt_stream.to_string data
        >>= fun data ->
            let headers = ref (Cohttp.Header.of_list [
                "Authorization", url.authorizationToken;
                "X-Bz-File-Name", fileName;
                "Content-Type", contentType;
                "Content-Length", string_of_int (String.length data);
                "X-Bz-Content-Sha1", hash_string data;
            ]) in
            let _ = List.iteri (fun i (k, v) ->
                headers := Cohttp.Header.replace !headers ("X-Bz-Info-" ^ k) (Ezjsonm.to_string v)) fileInfo in
            B2_client.post ~headers:!headers ~body:(`String data) (endpoint "upload_file")
        >|= Ezjsonm.from_string
        >|= handle_error find_all_file_info

    (* Large files *)

    let cancel_large_file token (fileId : string) : file Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O ["fileId", `String fileId]) (endpoint "cancel_large_file")
        >|= handle_error (fun j ->{
            fileId = file_id j;
            accountId = account_id j;
            bucketId = bucket_id j;
            fileName = file_name j;
        })

    let start_large_file token ?contentType:(contentType="b2/x-auto") ?fileInfo:(fileInfo=[]) bucketId fileName : partial_file_info Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O [
            "bucketId", `String bucketId;
            "contentType", `String contentType;
            "fileInfo", `O fileInfo;
        ]) (endpoint "start_large_file")
        >|= handle_error find_all_partial_file_info

    let finish_large_file token fileId partSha1Array : file_info Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O [
            "fileId", `String fileId;
            "partSha1Array", `A (List.map (fun s ->
                `String (Cstruct.to_string s))partSha1Array );
        ]) (endpoint "finish_large_file")
        >|= handle_error find_all_file_info

    type part = {
        fileId: string;
        partNumber: int;
        contentLength: int64;
        contentSha1: Cstruct.t;
    }

    let upload_part (url : upload_url) ?contentType:(contentType="b2/x-auto") (data : char Lwt_stream.t) partNum  : part Lwt.t =
        Lwt_stream.to_string data
        >>= fun data ->
            let headers = ref (Cohttp.Header.of_list [
                "Authorization", url.authorizationToken;
                "X-Bz-Part-Number", string_of_int partNum;
                "Content-Length", string_of_int (String.length data);
                "X-Bz-Content-Sha1", hash_string data;
            ]) in
            B2_client.post ~headers:!headers ~body:(`String data) (endpoint "upload_part")
        >|= Ezjsonm.from_string
        >|= handle_error (fun j -> {
            fileId = file_id j;
            partNumber = find_int j ["partNumber"] |> Int64.to_int;
            contentLength = find_int j ["contentLength"];
            contentSha1 = find_cstruct j ["contentSha1"];
        })

    let list_parts token fileId : part list Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O [
            "fileId", `String fileId;
            "maxPartCount", `String "1000";
        ]) (endpoint "list_parts")
        >|= handle_error (fun j ->
            Ezjsonm.find j ["parts"]
            |> Ezjsonm.get_list (fun j -> {
                fileId = file_id j;
                partNumber = find_int j ["partNumber"] |> Int64.to_int;
                contentLength = find_int j ["contentLength"];
                contentSha1 = find_cstruct j ["contentSha1"];
            }))


    (* Buckets *)

    (* NOTE: lifecycleRules is not yet implemented *)
    type bucket = {
        accountId: string;
        bucketId: string;
        bucketName: string;
        bucketType: string;
        bucketInfo: Ezjsonm.value;
        revision: Int64.t;
    }

    let find_all_bucket j = {
        accountId = account_id j;
        bucketId = bucket_id j;
        bucketName = bucket_name j;
        bucketType = bucket_type j;
        bucketInfo = bucket_info j;
        revision = revision j;
    }

    let create_bucket token ?bucketInfo (bucketName: string) (bucketType: string) : bucket Lwt.t =
        let headers = make_header token in
        let info = match bucketInfo with
            | Some jv -> ["bucketInfo", `O jv]
            | None -> []  in
        B2_client.post_json ~headers ~json:(`O ([
            "bucketName", `String bucketName;
            "bucketType", `String bucketType;
            "accountId", `String token.accountId;
        ] @ info) ) (endpoint "create_bucket")
        >|= handle_error find_all_bucket

    let delete_bucket token (bucketId: string) : bucket Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O [
            "bucketId", `String bucketId;
            "accountId", `String token.accountId;
        ]) (endpoint "delete_bucket")
        >|= handle_error find_all_bucket

    let get_upload_url token bucketId : upload_url Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O [
            "bucketId", `String bucketId;
        ]) (endpoint "get_upload_url")
        >|= handle_error (fun j -> {
            uploadUrl = find_string j ["uploadUrl"];
            authorizationToken = find_string j ["authorizationToken"];
        })

    let get_upload_part_url token fileId : upload_url Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O [
            "fileId", `String fileId;
        ]) (endpoint "get_upload_part_url")
        >|= handle_error (fun j -> {
            uploadUrl = find_string j ["uploadUrl"];
            authorizationToken = find_string j ["authorizationToken"];
        })

    let list_buckets token : bucket list Lwt.t =
        let headers = make_header token in
        B2_client.post_json ~headers ~json:(`O [
            "accountId", `String token.accountId
        ]) (endpoint "list_buckets")
        >|= handle_error (fun j ->
            Ezjsonm.find j ["buckets"]
            |> Ezjsonm.get_list find_all_bucket)

    let update_bucket token ?bucketType ?bucketInfo ?ifRevisionIs bucketId bucketName : bucket Lwt.t =
        let headers = make_header token in
        let params = ref [
            "accountId", `String token.accountId;
            "bucketId", `String bucketId;
            "bucketName", `String bucketName;
        ] in
        let _ = match bucketType with Some p -> (params := ("bucketType", `String p)::!params) | None -> () in
        let _ = match bucketInfo with Some p -> (params := ("bucketInfo", `O p)::!params) | None -> () in
        let _ = match ifRevisionIs with Some p -> (params := ("bucketName", `Float (Int64.to_float p))::!params) | None -> () in
        B2_client.post_json ~headers ~json:(`O !params) (endpoint "update_bucket")
        >|= handle_error find_all_bucket

end