(* TODO:
 * - list_unfinished_large_files *)

open Lwt
open B2_client

let auth_endpoint = "https://api.backblazeb2.com"
let mk_endpoint base path = base  ^ "/b2api/v1/b2_" ^ path


module V1 = struct

    module Token = struct
        type t = {
            accountId: string;
            authorizationToken: string;
            apiUrl: string;
            downloadUrl: string;
            minimumPartSize: int;
        }
    end

    module Download_authorization = struct
        type t = {
            bucketId: string;
            fileNamePrefix: string;
            authorizationToken: string;
        }
    end

    module File = struct
        type t = {
            fileId : string;
            accountId : string;
            bucketId : string;
            fileName : string;
        }
    end

    module File_version = struct
        type t = {
            fileId: string;
            fileName: string;
        }
    end

    module File_info = struct
        type t = {
            fileId: string;
            fileName: string;
            contentSha1: Cstruct.t;
            contentLength: int64;
            contentType: string;
            fileInfo: Ezjsonm.value;
            action: string;
            uploadTimestamp: Int64.t;
        }
    end

    module Partial_file_info = struct
        type t = {
            fileId: string;
            fileName: string;
            accountId: string;
            bucketId: string;
            contentType: string;
            fileInfo: Ezjsonm.value;
            uploadTimestamp: Int64.t;
        }
    end

    module Upload_url = struct
        type t = {
            uploadUrl: string;
            authorizationToken: string;
        }
    end

    module Part = struct
        type t = {
            fileId: string;
            partNumber: int;
            contentLength: int64;
            contentSha1: Cstruct.t;
        }
    end

    type bucket_type = [
        | `Public
        | `Private
    ]

    let string_of_bucket_type = function
        | `Public -> "allPublic"
        | `Private -> "allPrivate"

    (* Defaults to private when the string is invalid *)
    let bucket_type_of_string = function
        | "allPublic" -> `Public
        | _ -> `Private

    module Bucket = struct
        (* NOTE: lifecycleRules is not yet implemented *)
        type t = {
            accountId: string;
            bucketId: string;
            bucketName: string;
            bucketType: bucket_type;
            bucketInfo: Ezjsonm.value;
            revision: Int64.t;
        }
    end



    (* Makes a new header object with the given token *)
    let make_header ?fields:(fields=[]) token =
        Cohttp.Header.of_list ([
            "Authorization", token.Token.authorizationToken
        ] @ fields)

    let authorize_account (accountId: string) (applicationKey: string) =
        let encoded = Nocrypto.Base64.encode (Cstruct.of_string (accountId ^ ":" ^ applicationKey)) |> Cstruct.to_string in
        let headers = Cohttp.Header.of_list [
            "Authorization", "Basic " ^ encoded
        ] in
        B2_client.get_json headers (mk_endpoint auth_endpoint  "authorize_account")
        >|= handle_error (fun j -> Token.{
            accountId = account_id j;
            authorizationToken = find_string j ["authorizationToken"];
            apiUrl= find_string j ["apiUrl"];
            downloadUrl = find_string j ["downloadUrl"];
            minimumPartSize = find_int j ["minimumPartSize"] |> Int64.to_int;
        })

    (* Files *)

    let delete_file_version token (fileName : string) (fileId : string) =
        let headers = make_header token in
        B2_client.post_json ~json:(`O [
            "fileName", `String fileName;
            "fileId", `String fileId;
        ]) headers (mk_endpoint token.Token.apiUrl "delete_file_version")
        >|= handle_error (fun j -> File_version.{
            fileId = file_id j;
            fileName = file_name j;
        })

    let get_download_authorization token bucketId fileNamePrefix validDurationInSeconds =
        let headers = make_header token in
        B2_client.post_json ~json:(`O [
            "bucketId", `String bucketId;
            "fileNamePrefix", `String fileNamePrefix;
            "validDurationInSeconds", `String (string_of_int validDurationInSeconds);
        ]) headers (mk_endpoint token.Token.apiUrl "get_download_authorization")
        >|= handle_error (fun j -> Download_authorization.{
            bucketId = bucket_id j;
            fileNamePrefix = find_string j ["fileNamePrefix"];
            authorizationToken = find_string j ["authorizationToken"];
        })

    let download_file_by_id ?token ?url:(url="") ?range:(range="") (fileId: string) : string Lwt.t =
        let headers, url = match token with
        | Some tok ->
            make_header ~fields:["Range", range] tok, tok.Token.downloadUrl
        | None -> Cohttp.Header.init (), url in
        B2_client.get headers (mk_endpoint url ("download_file_by_id?fileId=" ^ fileId))

    let download_file_by_name ?token ?auth ?url:(url="") ?range:(range="") (fileName: string) : string Lwt.t =
        let headers, url = match token with
        | Some tok ->
            make_header tok, tok.Token.downloadUrl
        | None -> (match auth with
        | Some a -> Cohttp.Header.of_list [
            "Authorization", a.Download_authorization.authorizationToken;
            "Range", range;
        ]
        | None -> Cohttp.Header.init ()), url in
        B2_client.get headers (Filename.concat url fileName)

    let find_all_file_info j = File_info.{
        fileId = file_id j;
        fileName = file_name j;
        contentSha1 = find_cstruct j ["contentSha1"];
        contentLength = find_default find_int j ["contentLength"] 0L;
        contentType = find_string j ["contentType"];
        fileInfo = find_default Ezjsonm.find j ["fileInfo"] `Null;
        action = find_string j ["action"];
        uploadTimestamp = find_int j ["uploadTimestamp"];
    }

    let find_all_partial_file_info j = Partial_file_info.{
        fileId = file_id j;
        fileName = file_name j;
        accountId = account_id j;
        bucketId = bucket_id j;
        contentType = find_string j ["contentType"];
        fileInfo = find_default Ezjsonm.find j ["fileInfo"] `Null;
        uploadTimestamp = find_int j ["uploadTimestamp"];
    }

    let get_file_info token fileId =
        B2_client.post_json ~json:(`O [
            "fileId", `String fileId;
        ]) (make_header token) (mk_endpoint token.Token.apiUrl "get_file_info")
        >|= handle_error find_all_file_info

    let hide_file token bucketId fileName =
        let headers = make_header token in
        B2_client.post_json ~json:(`O [
            "bucketId", `String bucketId;
            "fileName", `String fileName;
        ]) headers (mk_endpoint token.Token.apiUrl "hide_file")
        >|= handle_error find_all_file_info

    let list_file_names token ?startFileName:(startFileName="")
                              ?maxFileCount:(maxFileCount=0)
                              ?prefix:(prefix="")
                              ?delimiter bucketId =
        let headers = make_header token in
        B2_client.post_json ~json:(`O ([
            "bucketId", `String bucketId;
            "maxFileCount", `Float (float_of_int maxFileCount);
            "prefix", `String prefix;
            "delimiter", (match delimiter with
            | Some d -> `String d
            | None -> `Null);
        ] @ (if startFileName <> "" then [
            "startFileName", `String startFileName;
        ] else []))) headers (mk_endpoint token.Token.apiUrl "list_file_names")
        >|= handle_error (fun j ->
            Ezjsonm.find j ["files"]
            |> Ezjsonm.get_list find_all_file_info
            |> fun l -> l, find_default find_string j ["nextFileName"] "")

    let list_file_versions token ?startFileName:(startFileName="")
                                 ?startFileId:(startFileId="")
                                 ?maxFileCount:(maxFileCount=0)
                                 ?prefix:(prefix="")
                                 ?delimiter bucketId =
        let headers = make_header token in
        B2_client.post_json ~json:(`O ([
            "bucketId", `String bucketId;
            "maxFileCount", `Float (float_of_int maxFileCount);
            "prefix", `String prefix;
            "delimiter", (match delimiter with
            | Some d -> `String d
            | None -> `Null);
        ] @ (if startFileName <> "" && startFileId <> "" then [
            "startFileName", `String startFileName;
            "startFileId", `String startFileId
        ] else []))) headers (mk_endpoint token.Token.apiUrl "list_file_versions")
        >|= handle_error (fun j ->
            Ezjsonm.find j ["files"]
            |> Ezjsonm.get_list find_all_file_info
            |> fun l -> l, find_default find_string j ["nextFileName"] "", find_default find_string j ["nextFileId"] "")

    let get_hex = function
        | `Hex s -> s
        | _ -> ""

    let hash_string s =
        let h = Nocrypto.Hash.SHA1.init () in
        let _ = Nocrypto.Hash.SHA1.feed h (Cstruct.of_string s) in
        Nocrypto.Hash.SHA1.get h |> Hex.of_cstruct |> get_hex

    let upload_file (url : Upload_url.t) ?contentType:(contentType="b2/x-auto") ?fileInfo:(fileInfo=[]) (data : char Lwt_stream.t) fileName  =

        Lwt_stream.to_string data
        >>= fun data ->
        let headers = ref (Cohttp.Header.of_list Upload_url.[
            "Authorization", url.authorizationToken;
            "X-Bz-File-Name", fileName;
            "Content-Type", contentType;
            "Content-Length", string_of_int (String.length data);
            "X-Bz-Content-Sha1", hash_string data;
        ]) in
        let _ = List.iteri (fun i (k, v) ->
            headers := Cohttp.Header.replace !headers ("X-Bz-Info-" ^ k) (Ezjsonm.to_string v)) fileInfo in
        B2_client.post ~body:(`String data) !headers url.Upload_url.uploadUrl
        >|= Ezjsonm.from_string
        >|= handle_error find_all_file_info

    (* Large files *)

    let cancel_large_file token (fileId : string)  =
        let headers = make_header token in
        B2_client.post_json ~json:(`O ["fileId", `String fileId]) headers (mk_endpoint token.Token.apiUrl "cancel_large_file")
        >|= handle_error (fun j -> File.{
            fileId = file_id j;
            accountId = account_id j;
            bucketId = bucket_id j;
            fileName = file_name j;
        })

    let start_large_file token ?contentType:(contentType="b2/x-auto") ?fileInfo:(fileInfo=[]) bucketId fileName =
        let headers = make_header token in
        B2_client.post_json  ~json:(`O [
            "bucketId", `String bucketId;
            "contentType", `String contentType;
            "fileInfo", `O fileInfo;
        ]) headers (mk_endpoint token.Token.apiUrl "start_large_file")
        >|= handle_error find_all_partial_file_info

    let finish_large_file token fileId partSha1Array =
        let headers = make_header token in
        B2_client.post_json ~json:(`O [
            "fileId", `String fileId;
            "partSha1Array", `A (List.map (fun s ->
                `String (Cstruct.to_string s))partSha1Array );
        ]) headers (mk_endpoint token.Token.apiUrl "finish_large_file")
        >|= handle_error find_all_file_info

    let upload_part (url : Upload_url.t) ?contentType:(contentType="b2/x-auto") (data : char Lwt_stream.t) partNum =
        Lwt_stream.to_string data
        >>= fun data ->
        let headers = ref (Cohttp.Header.of_list [
            "Authorization", url.Upload_url.authorizationToken;
            "X-Bz-Part-Number", string_of_int partNum;
            "Content-Length", string_of_int (String.length data);
            "X-Bz-Content-Sha1", hash_string data;
        ]) in
        B2_client.post ~body:(`String data) !headers url.Upload_url.uploadUrl
        >|= Ezjsonm.from_string
        >|= handle_error (fun j -> Part.{
            fileId = file_id j;
            partNumber = find_int j ["partNumber"] |> Int64.to_int;
            contentLength = find_int j ["contentLength"];
            contentSha1 = find_cstruct j ["contentSha1"];
        })

    let list_parts token fileId =
        let headers = make_header token in
        B2_client.post_json ~json:(`O [
            "fileId", `String fileId;
            "maxPartCount", `String "1000";
        ]) headers (mk_endpoint token.Token.apiUrl "list_parts")
        >|= handle_error (fun j ->
            Ezjsonm.find j ["parts"]
            |> Ezjsonm.get_list (fun j -> Part.{
                fileId = file_id j;
                partNumber = find_int j ["partNumber"] |> Int64.to_int;
                contentLength = find_int j ["contentLength"];
                contentSha1 = find_cstruct j ["contentSha1"];
            }))


    (* Buckets *)

    let find_all_bucket j = Bucket.{
        accountId = account_id j;
        bucketId = bucket_id j;
        bucketName = bucket_name j;
        bucketType = bucket_type j |> bucket_type_of_string;
        bucketInfo = bucket_info j;
        revision = revision j;
    }

    let create_bucket token ?bucketInfo (bucketName: string) (bucketType: bucket_type) =
        let headers = make_header token in
        let info = match bucketInfo with
            | Some jv -> ["bucketInfo", `O jv]
            | None -> []  in
        B2_client.post_json ~json:(`O ([
            "bucketName", `String bucketName;
            "bucketType", `String (string_of_bucket_type bucketType);
            "accountId", `String token.Token.accountId;
        ] @ info)) headers (mk_endpoint token.Token.apiUrl "create_bucket")
        >|= handle_error find_all_bucket

    let delete_bucket token (bucketId: string) =
        let headers = make_header token in
        B2_client.post_json ~json:(`O [
            "bucketId", `String bucketId;
            "accountId", `String token.Token.accountId;
        ]) headers (mk_endpoint token.Token.apiUrl "delete_bucket")
        >|= handle_error find_all_bucket

    let get_upload_url token bucketId =
        let headers = make_header token in
        B2_client.post_json ~json:(`O [
            "bucketId", `String bucketId;
        ]) headers (mk_endpoint token.Token.apiUrl "get_upload_url")
        >|= handle_error (fun j -> Upload_url.{
            uploadUrl = find_string j ["uploadUrl"];
            authorizationToken = find_string j ["authorizationToken"];
        })

    let get_upload_part_url token fileId =
        let headers = make_header token in
        B2_client.post_json ~json:(`O [
            "fileId", `String fileId;
        ]) headers (mk_endpoint token.Token.apiUrl "get_upload_part_url")
        >|= handle_error (fun j -> Upload_url.{
            uploadUrl = find_string j ["uploadUrl"];
            authorizationToken = find_string j ["authorizationToken"];
        })

    let list_buckets token =
        let headers = make_header token in
        B2_client.post_json ~json:(`O [
            "accountId", `String token.Token.accountId
        ]) headers (mk_endpoint token.Token.apiUrl "list_buckets")
        >|= handle_error (fun j ->
            Ezjsonm.find j ["buckets"]
            |> Ezjsonm.get_list find_all_bucket)

    let update_bucket token ?bucketType ?bucketInfo ?ifRevisionIs bucketId bucketName =
        let headers = make_header token in
        let params = ref [
            "accountId", `String token.Token.accountId;
            "bucketId", `String bucketId;
            "bucketName", `String bucketName;
        ] in
        let _ = match bucketType with Some p -> (params := ("bucketType", `String (string_of_bucket_type p))::!params) | None -> () in
        let _ = match bucketInfo with Some p -> (params := ("bucketInfo", `O p)::!params) | None -> () in
        let _ = match ifRevisionIs with Some p -> (params := ("bucketName", `Float (Int64.to_float p))::!params) | None -> () in
        B2_client.post_json ~json:(`O !params) headers (mk_endpoint token.Token.apiUrl "update_bucket")
        >|= handle_error find_all_bucket

end
