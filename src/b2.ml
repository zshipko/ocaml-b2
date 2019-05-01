(* TODO:
 * - list_unfinished_large_files *)

open Lwt
open B2_client

let auth_endpoint = "https://api.backblazeb2.com"

let mk_endpoint base path = base ^ "/b2api/v1/b2_" ^ path

module V1 (C : Cohttp_lwt.S.Client) = struct
  module IO = B2_client.IO (C)

  module Token = struct
    type t = {
      account_id : string;
      authorization_token : string;
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
      content_sha1 : Cstruct.t;
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
      content_sha1 : Cstruct.t
    }
  end

  type bucket_type = [ `Public | `Private ]

  let string_of_bucket_type = function
    | `Public -> "all_public"
    | `Private -> "all_private"

  (* Defaults to private when the string is invalid *)
  let bucket_type_of_string = function
    | "all_public" -> `Public
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

  let authorize_account (account_id : string) (application_key : string) =
    let encoded =
      Nocrypto.Base64.encode
        (Cstruct.of_string (account_id ^ ":" ^ application_key))
      |> Cstruct.to_string
    in
    let headers =
      Cohttp.Header.of_list [ ("Authorization", "Basic " ^ encoded) ]
    in
    IO.get_json headers (mk_endpoint auth_endpoint "authorize_account")
    >|= handle_error (fun j ->
            Token.
              { account_id;
                authorization_token = find_string j [ "authorization_token" ];
                api_url = find_string j [ "api_url" ];
                download_url = find_string j [ "download_url" ];
                minimum_part_size =
                  find_int j [ "minimum_part_size" ] |> Int64.to_int
              } )

  (* Files *)

  let delete_file_version token (file_name : string) (file_id : string) =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O [ ("file_name", `String file_name); ("file_id", `String file_id) ])
      headers
      (mk_endpoint token.Token.api_url "delete_file_version")
    >|= handle_error (fun _j -> File_version.{ file_id; file_name })

  let get_download_authorization token bucket_id file_name_prefix
      valid_duration_in_seconds =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          [ ("bucket_id", `String bucket_id);
            ("file_name_prefix", `String file_name_prefix);
            ( "valid_duration_in_seconds",
              `String (string_of_int valid_duration_in_seconds) )
          ])
      headers
      (mk_endpoint token.Token.api_url "get_download_authorization")
    >|= handle_error (fun j ->
            Download_authorization.
              { bucket_id;
                file_name_prefix = find_string j [ "file_name_prefix" ];
                authorization_token = find_string j [ "authorization_token" ]
              } )

  let download_file_by_id ?token ?(url = "") ?(range = "") (file_id : string) :
      string Lwt.t =
    let headers, url =
      match token with
      | Some tok ->
          (make_header ~fields:[ ("Range", range) ] tok, tok.Token.download_url)
      | None -> (Cohttp.Header.init (), url)
    in
    IO.get headers (mk_endpoint url ("download_file_by_id?file_id=" ^ file_id))

  let download_file_by_name ?token ?auth ?(url = "") ?(range = "")
      (file_name : string) : string Lwt.t =
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
        content_sha1 = find_cstruct j [ "content_sha1" ];
        content_length = find_default find_int j [ "content_length" ] 0L;
        content_type = find_string j [ "content_type" ];
        file_info = find_default Ezjsonm.find j [ "file_info" ] `Null;
        action = find_string j [ "action" ];
        upload_timestamp = find_int j [ "upload_timestamp" ]
      }

  let find_all_partial_file_info j =
    Partial_file_info.
      { file_id = file_id j;
        file_name = file_name j;
        account_id = account_id j;
        bucket_id = bucket_id j;
        content_type = find_string j [ "content_type" ];
        file_info = find_default Ezjsonm.find j [ "file_info" ] `Null;
        upload_timestamp = find_int j [ "upload_timestamp" ]
      }

  let get_file_info token file_id =
    IO.post_json
      ~json:(`O [ ("file_id", `String file_id) ])
      (make_header token)
      (mk_endpoint token.Token.api_url "get_file_info")
    >|= handle_error find_all_file_info

  let hide_file token bucket_id file_name =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          [ ("bucket_id", `String bucket_id); ("file_name", `String file_name) ])
      headers
      (mk_endpoint token.Token.api_url "hide_file")
    >|= handle_error find_all_file_info

  let list_file_names token ?(start_file_name = "") ?(max_file_count = 0)
      ?(prefix = "") ?delimiter bucket_id =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          ( [ ("bucket_id", `String bucket_id);
              ("max_file_count", `Float (float_of_int max_file_count));
              ("prefix", `String prefix);
              ( "delimiter",
                match delimiter with Some d -> `String d | None -> `Null )
            ]
          @
          if start_file_name <> "" then
            [ ("start_file_name", `String start_file_name) ]
          else [] ))
      headers
      (mk_endpoint token.Token.api_url "list_file_names")
    >|= handle_error (fun j ->
            Ezjsonm.find j [ "files" ] |> Ezjsonm.get_list find_all_file_info
            |> fun l -> (l, find_default find_string j [ "next_file_name" ] "")
        )

  let list_file_versions token ?(start_file_name = "") ?(start_file_id = "")
      ?(max_file_count = 0) ?(prefix = "") ?delimiter bucket_id =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          ( [ ("bucket_id", `String bucket_id);
              ("max_file_count", `Float (float_of_int max_file_count));
              ("prefix", `String prefix);
              ( "delimiter",
                match delimiter with Some d -> `String d | None -> `Null )
            ]
          @
          if start_file_name <> "" && start_file_id <> "" then
            [ ("start_file_name", `String start_file_name);
              ("start_file_id", `String start_file_id)
            ]
          else [] ))
      headers
      (mk_endpoint token.Token.api_url "list_file_versions")
    >|= handle_error (fun j ->
            Ezjsonm.find j [ "files" ] |> Ezjsonm.get_list find_all_file_info
            |> fun l ->
            ( l,
              find_default find_string j [ "next_file_name" ] "",
              find_default find_string j [ "next_file_id" ] "" ) )

  let get_hex = function `Hex s -> s | _ -> ""

  let hash_string s =
    let h = Nocrypto.Hash.SHA1.init () in
    let _ = Nocrypto.Hash.SHA1.feed h (Cstruct.of_string s) in
    Nocrypto.Hash.SHA1.get h |> Hex.of_cstruct |> get_hex

  let upload_file (url : Upload_url.t) ?(content_type = "b2/x-auto")
      ?(file_info = []) (data : char Lwt_stream.t) file_name =
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

  let cancel_large_file token (file_id : string) =
    let headers = make_header token in
    IO.post_json
      ~json:(`O [ ("file_id", `String file_id) ])
      headers
      (mk_endpoint token.Token.api_url "cancel_large_file")
    >|= handle_error (fun j ->
            File.
              { file_id;
                account_id = account_id j;
                bucket_id = bucket_id j;
                file_name = file_name j
              } )

  let start_large_file token ?(content_type = "b2/x-auto") ?(file_info = [])
      bucket_id file_name =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          [ ("bucket_id", `String bucket_id);
            ("content_type", `String content_type);
            ("file_info", `O file_info);
            ("file_name", `String file_name)
          ])
      headers
      (mk_endpoint token.Token.api_url "start_large_file")
    >|= handle_error find_all_partial_file_info

  let finish_large_file token file_id part_sha1_array =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          [ ("file_id", `String file_id);
            ( "part_sha1_array",
              `A
                (List.map
                   (fun s -> `String (Cstruct.to_string s))
                   part_sha1_array) )
          ])
      headers
      (mk_endpoint token.Token.api_url "finish_large_file")
    >|= handle_error find_all_file_info

  let upload_part (url : Upload_url.t) (data : char Lwt_stream.t) part_num =
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
                part_number = find_int j [ "part_number" ] |> Int64.to_int;
                content_length = find_int j [ "content_length" ];
                content_sha1 = find_cstruct j [ "content_sha1" ]
              } )

  let list_parts token file_id =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          [ ("file_id", `String file_id); ("max_part_count", `String "1000") ])
      headers
      (mk_endpoint token.Token.api_url "list_parts")
    >|= handle_error (fun j ->
            Ezjsonm.find j [ "parts" ]
            |> Ezjsonm.get_list (fun j ->
                   Part.
                     { file_id;
                       part_number =
                         find_int j [ "part_number" ] |> Int64.to_int;
                       content_length = find_int j [ "content_length" ];
                       content_sha1 = find_cstruct j [ "content_sha1" ]
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

  let create_bucket token ?bucket_info (bucket_name : string)
      (bucket_type : bucket_type) =
    let headers = make_header token in
    let info =
      match bucket_info with
      | Some jv -> [ ("bucket_info", `O jv) ]
      | None -> []
    in
    IO.post_json
      ~json:
        (`O
          ( [ ("bucket_name", `String bucket_name);
              ("bucket_type", `String (string_of_bucket_type bucket_type));
              ("account_id", `String token.Token.account_id)
            ]
          @ info ))
      headers
      (mk_endpoint token.Token.api_url "create_bucket")
    >|= handle_error find_all_bucket

  let delete_bucket token (bucket_id : string) =
    let headers = make_header token in
    IO.post_json
      ~json:
        (`O
          [ ("bucket_id", `String bucket_id);
            ("account_id", `String token.Token.account_id)
          ])
      headers
      (mk_endpoint token.Token.api_url "delete_bucket")
    >|= handle_error find_all_bucket

  let get_upload_url token bucket_id =
    let headers = make_header token in
    IO.post_json
      ~json:(`O [ ("bucket_id", `String bucket_id) ])
      headers
      (mk_endpoint token.Token.api_url "get_upload_url")
    >|= handle_error (fun j ->
            Upload_url.
              { upload_url = find_string j [ "upload_url" ];
                authorization_token = find_string j [ "authorization_token" ]
              } )

  let get_upload_part_url token file_id =
    let headers = make_header token in
    IO.post_json
      ~json:(`O [ ("file_id", `String file_id) ])
      headers
      (mk_endpoint token.Token.api_url "get_upload_part_url")
    >|= handle_error (fun j ->
            Upload_url.
              { upload_url = find_string j [ "upload_url" ];
                authorization_token = find_string j [ "authorization_token" ]
              } )

  let list_buckets token =
    let headers = make_header token in
    IO.post_json
      ~json:(`O [ ("account_id", `String token.Token.account_id) ])
      headers
      (mk_endpoint token.Token.api_url "list_buckets")
    >|= handle_error (fun j ->
            Ezjsonm.find j [ "buckets" ] |> Ezjsonm.get_list find_all_bucket )

  let update_bucket token ?bucket_type ?bucket_info ?if_revision_is bucket_id
      bucket_name =
    let headers = make_header token in
    let params =
      ref
        [ ("account_id", `String token.Token.account_id);
          ("bucket_id", `String bucket_id);
          ("bucket_name", `String bucket_name)
        ]
    in
    let _ =
      match bucket_type with
      | Some p ->
          params :=
            ("bucket_type", `String (string_of_bucket_type p)) :: !params
      | None -> ()
    in
    let _ =
      match bucket_info with
      | Some p -> params := ("bucket_info", `O p) :: !params
      | None -> ()
    in
    let _ =
      match if_revision_is with
      | Some p ->
          params := ("bucket_name", `Float (Int64.to_float p)) :: !params
      | None -> ()
    in
    IO.post_json ~json:(`O !params) headers
      (mk_endpoint token.Token.api_url "update_bucket")
    >|= handle_error find_all_bucket
end
