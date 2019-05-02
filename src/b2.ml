(* TODO:
 * - list_unfinished_large_files *)

open Lwt

let auth_endpoint = "https://api.backblazeb2.com"

let mk_endpoint base path = base ^ "/b2api/v1/b2_" ^ path

module IO (C : Cohttp_lwt.S.Client) = struct
  let get ?ctx headers url =
    C.get ?ctx ~headers (Uri.of_string url) >>= fun (_res, body) ->
    Cohttp_lwt.Body.to_string body

  let post ?ctx ?(body = `Empty) headers url =
    Cohttp_lwt.Body.length body >>= fun (len, body) ->
    let headers =
      Cohttp.Header.replace headers "Content-Length" (Int64.to_string len)
    in
    C.post ?ctx ~headers ~body (Uri.of_string url) >>= fun (_res, body) ->
    Cohttp_lwt.Body.to_string body

  (*let post_form ?ctx ?(params = []) headers url =
    let len = Uri.encoded_of_query params |> String.length in
    let headers =
      Cohttp.Header.replace headers "Content-Length" (string_of_int len)
    in
    C.post_form ?ctx ~headers ~params (Uri.of_string url)
    >>= fun (_res, body) -> Cohttp_lwt.Body.to_string body*)

  let post_json ?ctx ?json headers url =
    let body, _len =
      match json with
      | Some j ->
          let s = Ezjsonm.to_string j in
          (`String s, String.length s)
      | None -> (`Empty, 0)
    in
    post ?ctx ~body headers url >|= Ezjsonm.from_string

  let get_json ?ctx headers url = get ?ctx headers url >|= Ezjsonm.from_string

  (*let post_form_json ?ctx ~params headers url =
    post_form ?ctx ~params headers url >|= Ezjsonm.from_string*)
end

let find_default fn j name d = try fn j name with _ -> d

let find_string j name = Ezjsonm.find j name |> Ezjsonm.get_string

let find_int j name = Ezjsonm.find j name |> Ezjsonm.get_int64

let bucket_id j = find_string j [ "bucketId" ]

let file_id j = find_string j [ "fileId" ]

let file_name j = find_string j [ "fileName" ]

let account_id j = find_string j [ "accountId" ]

let bucket_name j = find_string j [ "bucketName" ]

let bucket_type j = find_string j [ "bucketType" ]

let bucket_info j = try Ezjsonm.find j [ "bucketInfo" ] with _ -> `Null

let revision j = find_int j [ "revision" ]

type error = { status : int; code : string; message : string }

exception Error_response of error

exception API_error

let handle_error x j =
  try x j
  with exc ->
    raise
      ( try
          Error_response
            { status = find_int j [ "status" ] |> Int64.to_int;
              code = find_string j [ "code" ];
              message = find_string j [ "message" ]
            }
        with _ -> raise exc )

module V1 (C : Cohttp_lwt.S.Client) = struct
  module IO = IO (C)

  module Token = struct
    type t = {
      key_id : string;
      authorization_token : string;
      account_id : string;
      api_url : string;
      download_url : string;
      minimum_part_size : int
    }
  end

  module Download_authorization = struct
    type t = {
      bucket_id : string;
      file_name_prefix : string;
      authorization_token : string
    }
  end

  module File = struct
    type t = {
      file_id : string;
      account_id : string;
      bucket_id : string;
      file_name : string
    }
  end

  module File_version = struct
    type t = { file_id : string; file_name : string }
  end

  module File_info = struct
    type t = {
      file_id : string;
      file_name : string;
      content_sha1 : string;
      content_length : int64;
      content_type : string;
      file_info : Ezjsonm.value;
      action : string;
      upload_timestamp : Int64.t
    }
  end

  module Partial_file_info = struct
    type t = {
      file_id : string;
      file_name : string;
      account_id : string;
      bucket_id : string;
      content_type : string;
      file_info : Ezjsonm.value;
      upload_timestamp : Int64.t
    }
  end

  module Upload_url = struct
    type t = { upload_url : string; authorization_token : string }
  end

  module Part = struct
    type t = {
      file_id : string;
      part_number : int;
      content_length : int64;
      content_sha1 : string
    }
  end

  type bucket_type = [ `Public | `Private ]

  let string_of_bucket_type = function
    | `Public -> "allPublic"
    | `Private -> "allPrivate"

  (* Defaults to private when the string is invalid *)
  let bucket_type_of_string = function
    | "allPublic" | "ALLPUBLIC" | "public" | "PUBLIC" -> `Public
    | _ -> `Private

  module Bucket = struct
    (* NOTE: lifecycle_rules is not yet implemented *)
    type t = {
      account_id : string;
      bucket_id : string;
      bucket_name : string;
      bucket_type : bucket_type;
      bucket_info : Ezjsonm.value;
      revision : Int64.t
    }
  end

  (* Makes a new header object with the given token *)
  let make_header ?(fields = []) token =
    Cohttp.Header.of_list
      ([ ("Authorization", token.Token.authorization_token) ] @ fields)

  let authorize_account ~key_id ~application_key =
    let encoded = Base64.encode_string (key_id ^ ":" ^ application_key) in
    let headers =
      Cohttp.Header.of_list [ ("Authorization", "Basic " ^ encoded) ]
    in
    IO.get_json headers (mk_endpoint auth_endpoint "authorize_account")
    >|= handle_error (fun j ->
            Token.
              { key_id;
                account_id = account_id j;
                authorization_token = find_string j [ "authorizationToken" ];
                api_url = find_string j [ "apiUrl" ];
                download_url = find_string j [ "downloadUrl" ];
                minimum_part_size =
                  find_int j [ "minimumPartSize" ] |> Int64.to_int
              } )

  (* Files *)

  let delete_file_version ~token ~file_name ~file_id =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O [ ("fileName", `String file_name); ("fileId", `String file_id) ])
      headers
      (mk_endpoint token.Token.api_url "delete_file_version")
    >|= handle_error (fun _j -> File_version.{ file_id; file_name })

  let get_download_authorization ~token ~bucket_id ~file_name_prefix
      ~valid_duration_in_seconds =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          [ ("bucketId", `String bucket_id);
            ("fileNamePrefix", `String file_name_prefix);
            ( "validDurationInSeconds",
              `String (string_of_int valid_duration_in_seconds) )
          ])
      headers
      (mk_endpoint token.Token.api_url "get_download_authorization")
    >|= handle_error (fun j ->
            Download_authorization.
              { bucket_id;
                file_name_prefix = find_string j [ "fileNamePrefix" ];
                authorization_token = find_string j [ "authorizationToken" ]
              } )

  let download_file_by_id ?token ?(url = "") ?(range = "") ~file_id () :
      string Lwt.t =
    let headers, url =
      match token with
      | Some tok ->
          (make_header ~fields:[ ("range", range) ] tok, tok.Token.download_url)
      | None -> (Cohttp.Header.init (), url)
    in
    IO.get headers (mk_endpoint url ("download_file_by_id?fileId=" ^ file_id))

  let download_file_by_name ?token ?auth ?(url = "") ?(range = "") ~file_name
      () : string Lwt.t =
    let headers, url =
      match token with
      | Some tok -> (make_header tok, tok.Token.download_url)
      | None ->
          ( ( match auth with
            | Some a ->
                Cohttp.Header.of_list
                  [ ( "Authorization",
                      a.Download_authorization.authorization_token );
                    ("Range", range)
                  ]
            | None -> Cohttp.Header.init () ),
            url )
    in
    IO.get headers (Filename.concat url file_name)

  let find_all_file_info j =
    File_info.
      { file_id = file_id j;
        file_name = file_name j;
        content_sha1 = find_string j [ "contentSha1" ];
        content_length = find_default find_int j [ "contentLength" ] 0L;
        content_type = find_string j [ "contentType" ];
        file_info = find_default Ezjsonm.find j [ "fileInfo" ] `Null;
        action = find_string j [ "action" ];
        upload_timestamp = find_int j [ "uploadTimestamp" ]
      }

  let find_all_partial_file_info j =
    Partial_file_info.
      { file_id = file_id j;
        file_name = file_name j;
        account_id = account_id j;
        bucket_id = bucket_id j;
        content_type = find_string j [ "contentType" ];
        file_info = find_default Ezjsonm.find j [ "fileInfo" ] `Null;
        upload_timestamp = find_int j [ "uploadTimestamp" ]
      }

  let get_file_info ~token ~file_id =
    IO.post_json
      ~json:(`O [ ("fileId", `String file_id) ])
      (make_header token)
      (mk_endpoint token.Token.api_url "get_file_info")
    >|= handle_error find_all_file_info

  let hide_file ~token ~bucket_id ~file_name =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          [ ("bucketId", `String bucket_id); ("fileName", `String file_name) ])
      headers
      (mk_endpoint token.Token.api_url "hide_file")
    >|= handle_error find_all_file_info

  let list_file_names ~token ?(start_file_name = "") ?(max_file_count = 0)
      ?(prefix = "") ?delimiter ~bucket_id () =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          ( [ ("bucketId", `String bucket_id);
              ("maxFileCount", `Float (float_of_int max_file_count));
              ("prefix", `String prefix);
              ( "delimiter",
                match delimiter with Some d -> `String d | None -> `Null )
            ]
          @
          if start_file_name <> "" then
            [ ("startFileName", `String start_file_name) ]
          else [] ))
      headers
      (mk_endpoint token.Token.api_url "list_file_names")
    >|= handle_error (fun j ->
            Ezjsonm.find j [ "files" ] |> Ezjsonm.get_list find_all_file_info
            |> fun l -> (l, find_default find_string j [ "nextFileName" ] "")
        )

  let list_file_versions ~token ?(start_file_name = "") ?(start_file_id = "")
      ?(max_file_count = 0) ?(prefix = "") ?delimiter ~bucket_id () =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          ( [ ("bucketId", `String bucket_id);
              ("maxFileCount", `Float (float_of_int max_file_count));
              ("prefix", `String prefix);
              ( "delimiter",
                match delimiter with Some d -> `String d | None -> `Null )
            ]
          @
          if start_file_name <> "" && start_file_id <> "" then
            [ ("startFileName", `String start_file_name);
              ("startFileId", `String start_file_id)
            ]
          else [] ))
      headers
      (mk_endpoint token.Token.api_url "list_file_versions")
    >|= handle_error (fun j ->
            Ezjsonm.find j [ "files" ] |> Ezjsonm.get_list find_all_file_info
            |> fun l ->
            ( l,
              find_default find_string j [ "nextFileName" ] "",
              find_default find_string j [ "nextFileId" ] "" ) )

  let hash_string s = Digestif.SHA1.digest_string s |> Digestif.SHA1.to_hex

  let upload_file ~(url : Upload_url.t) ?(content_type = "b2/x-auto")
      ?(file_info = []) ~(data : char Lwt_stream.t) ~file_name () =
    Lwt_stream.to_string data >>= fun data ->
    let headers =
      ref
        (Cohttp.Header.of_list
           Upload_url.
             [ ("Authorization", url.authorization_token);
               ("X-Bz-File-Name", file_name);
               ("Content-Type", content_type);
               ("Content-Length", string_of_int (String.length data));
               ("X-Bz-Content-Sha1", hash_string data)
             ])
    in
    let _ =
      List.iter
        (fun (k, v) ->
          headers :=
            Cohttp.Header.replace !headers ("X-Bz-Info-" ^ k)
              (Ezjsonm.to_string v) )
        file_info
    in
    IO.post ~body:(`String data) !headers url.Upload_url.upload_url
    >|= Ezjsonm.from_string
    >|= handle_error find_all_file_info

  (* Large files *)

  let cancel_large_file ~token ~(file_id : string) =
    let headers = make_header token in
    IO.post_json
      ~json:(`O [ ("fileId", `String file_id) ])
      headers
      (mk_endpoint token.Token.api_url "cancel_large_file")
    >|= handle_error (fun j ->
            File.
              { file_id;
                account_id = account_id j;
                bucket_id = bucket_id j;
                file_name = file_name j
              } )

  let start_large_file ~token ?(content_type = "b2/x-auto") ?(file_info = [])
      ~bucket_id ~file_name () =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          [ ("bucketId", `String bucket_id);
            ("contentType", `String content_type);
            ("fileInfo", `O file_info);
            ("fileName", `String file_name)
          ])
      headers
      (mk_endpoint token.Token.api_url "start_large_file")
    >|= handle_error find_all_partial_file_info

  let finish_large_file ~token ~file_id ~part_sha1_array =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          [ ("fileId", `String file_id);
            ( "partSha1Array",
              `A (List.map (fun s -> `String s) part_sha1_array) )
          ])
      headers
      (mk_endpoint token.Token.api_url "finish_large_file")
    >|= handle_error find_all_file_info

  let upload_part ~(url : Upload_url.t) ~(data : char Lwt_stream.t) ~part_num =
    Lwt_stream.to_string data >>= fun data ->
    let headers =
      ref
        (Cohttp.Header.of_list
           [ ("Authorization", url.Upload_url.authorization_token);
             ("X-Bz-Part-Number", string_of_int part_num);
             ("Content-Length", string_of_int (String.length data));
             ("X-Bz-Content-Sha1", hash_string data)
           ])
    in
    IO.post ~body:(`String data) !headers url.Upload_url.upload_url
    >|= Ezjsonm.from_string
    >|= handle_error (fun j ->
            Part.
              { file_id = file_id j;
                part_number = find_int j [ "partNumber" ] |> Int64.to_int;
                content_length = find_int j [ "contentLength" ];
                content_sha1 = find_string j [ "contentSha1" ]
              } )

  let list_parts ~token ~file_id =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O [ ("fileId", `String file_id); ("maxPartCount", `String "1000") ])
      headers
      (mk_endpoint token.Token.api_url "list_parts")
    >|= handle_error (fun j ->
            Ezjsonm.find j [ "parts" ]
            |> Ezjsonm.get_list (fun j ->
                   Part.
                     { file_id;
                       part_number =
                         find_int j [ "partNumber" ] |> Int64.to_int;
                       content_length = find_int j [ "contentLength" ];
                       content_sha1 = find_string j [ "contentSha1" ]
                     } ) )

  (* Buckets *)

  let find_all_bucket j =
    Bucket.
      { account_id = account_id j;
        bucket_id = bucket_id j;
        bucket_name = bucket_name j;
        bucket_type = bucket_type j |> bucket_type_of_string;
        bucket_info = bucket_info j;
        revision = revision j
      }

  let create_bucket ~token ?bucket_info ~bucket_name ~bucket_type () =
    let headers = make_header token in
    let info =
      match bucket_info with
      | Some jv -> [ ("bucketInfo", `O jv) ]
      | None -> []
    in
    IO.post_json
      ~json:
        (`O
          ( [ ("bucketName", `String bucket_name);
              ("bucketType", `String (string_of_bucket_type bucket_type));
              ("accountId", `String token.Token.account_id)
            ]
          @ info ))
      headers
      (mk_endpoint token.Token.api_url "create_bucket")
    >|= handle_error find_all_bucket

  let delete_bucket ~token ~bucket_id =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          [ ("bucketId", `String bucket_id);
            ("accountId", `String token.Token.account_id)
          ])
      headers
      (mk_endpoint token.Token.api_url "delete_bucket")
    >|= handle_error find_all_bucket

  let get_upload_url ~token ~bucket_id =
    let headers = make_header token in
    IO.post_json
      ~json:(`O [ ("bucketId", `String bucket_id) ])
      headers
      (mk_endpoint token.Token.api_url "get_upload_url")
    >|= handle_error (fun j ->
            Upload_url.
              { upload_url = find_string j [ "uploadUrl" ];
                authorization_token = find_string j [ "authorizationToken" ]
              } )

  let get_upload_part_url ~token ~file_id =
    let headers = make_header token in
    IO.post_json
      ~json:(`O [ ("fileId", `String file_id) ])
      headers
      (mk_endpoint token.Token.api_url "get_upload_part_url")
    >|= handle_error (fun j ->
            Upload_url.
              { upload_url = find_string j [ "uploadUrl" ];
                authorization_token = find_string j [ "authorizationToken" ]
              } )

  let list_buckets ~token =
    let headers = make_header token in
    IO.post_json
      ~json:(`O [ ("accountId", `String token.Token.account_id) ])
      headers
      (mk_endpoint token.Token.api_url "list_buckets")
    >|= handle_error (fun j ->
            Ezjsonm.find j [ "buckets" ] |> Ezjsonm.get_list find_all_bucket )

  let update_bucket ~token ?bucket_type ?bucket_info ?if_revision_is ~bucket_id
      ~bucket_name () =
    let headers = make_header token in
    let params =
      ref
        [ ("accountId", `String token.Token.account_id);
          ("bucketId", `String bucket_id);
          ("bucketName", `String bucket_name)
        ]
    in
    let _ =
      match bucket_type with
      | Some p ->
          params :=
            ("bucketType", `String (string_of_bucket_type p)) :: !params
      | None -> ()
    in
    let _ =
      match bucket_info with
      | Some p -> params := ("bucketInfo", `O p) :: !params
      | None -> ()
    in
    let _ =
      match if_revision_is with
      | Some p ->
          params := ("bucketName", `Float (Int64.to_float p)) :: !params
      | None -> ()
    in
    IO.post_json ~json:(`O !params) headers
      (mk_endpoint token.Token.api_url "update_bucket")
    >|= handle_error find_all_bucket
end
