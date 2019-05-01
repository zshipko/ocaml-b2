module IO (C : Cohttp_lwt.S.Client) : sig
  val get : ?ctx:C.ctx -> Cohttp.Header.t -> string -> string Lwt.t

  val post :
    ?ctx:C.ctx ->
    ?body:Cohttp_lwt.Body.t ->
    Cohttp.Header.t ->
    string ->
    string Lwt.t

  val post_form :
    ?ctx:C.ctx ->
    ?params:(string * string list) list ->
    Cohttp.Header.t ->
    string ->
    string Lwt.t

  val post_json :
    ?ctx:C.ctx ->
    ?json:Ezjsonm.t ->
    Cohttp.Header.t ->
    string ->
    [> Ezjsonm.t ] Lwt.t

  val get_json :
    ?ctx:C.ctx -> Cohttp.Header.t -> string -> [> Ezjsonm.t ] Lwt.t

  val post_form_json :
    ?ctx:C.ctx ->
    params:(string * string list) list ->
    Cohttp.Header.t ->
    string ->
    [> Ezjsonm.t ] Lwt.t
end

type error = { status : int; code : string; message : string }

exception Error_response of error

exception API_error

module V1 (C : Cohttp_lwt.S.Client) : sig
  module IO : sig
    val get : ?ctx:C.ctx -> Cohttp.Header.t -> string -> string Lwt.t

    val post :
      ?ctx:C.ctx ->
      ?body:Cohttp_lwt.Body.t ->
      Cohttp.Header.t ->
      string ->
      string Lwt.t

    val post_form :
      ?ctx:C.ctx ->
      ?params:(string * string list) list ->
      Cohttp.Header.t ->
      string ->
      string Lwt.t

    val post_json :
      ?ctx:C.ctx ->
      ?json:Ezjsonm.t ->
      Cohttp.Header.t ->
      string ->
      [> Ezjsonm.t ] Lwt.t

    val get_json :
      ?ctx:C.ctx -> Cohttp.Header.t -> string -> [> Ezjsonm.t ] Lwt.t

    val post_form_json :
      ?ctx:C.ctx ->
      params:(string * string list) list ->
      Cohttp.Header.t ->
      string ->
      [> Ezjsonm.t ] Lwt.t
  end

  module Token : sig
    type t = {
      account_id : string;
      authorization_token : string;
      api_url : string;
      download_url : string;
      minimum_part_size : int
    }
  end

  module Download_authorization : sig
    type t = {
      bucket_id : string;
      file_name_prefix : string;
      authorization_token : string
    }
  end

  module File : sig
    type t = {
      file_id : string;
      account_id : string;
      bucket_id : string;
      file_name : string
    }
  end

  module File_version : sig
    type t = { file_id : string; file_name : string }
  end

  module File_info : sig
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

  module Partial_file_info : sig
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

  module Upload_url : sig
    type t = { upload_url : string; authorization_token : string }
  end

  module Part : sig
    type t = {
      file_id : string;
      part_number : int;
      content_length : int64;
      content_sha1 : Cstruct.t
    }
  end

  type bucket_type = [ `Private | `Public ]

  val string_of_bucket_type : [< `Private | `Public ] -> string

  val bucket_type_of_string : string -> [> `Private | `Public ]

  module Bucket : sig
    type t = {
      account_id : string;
      bucket_id : string;
      bucket_name : string;
      bucket_type : bucket_type;
      bucket_info : Ezjsonm.value;
      revision : Int64.t
    }
  end

  val make_header :
    ?fields:(string * string) list -> Token.t -> Cohttp.Header.t

  val authorize_account :
    account_id:string -> application_key:string -> Token.t Lwt.t

  val delete_file_version :
    token:Token.t -> file_name:string -> file_id:string -> File_version.t Lwt.t

  val get_download_authorization :
    token:Token.t ->
    bucket_id:string ->
    file_name_prefix:string ->
    int ->
    Download_authorization.t Lwt.t

  val download_file_by_id :
    ?token:Token.t ->
    ?url:string ->
    ?range:string ->
    file_id:string ->
    string Lwt.t

  val download_file_by_name :
    ?token:Token.t ->
    ?auth:Download_authorization.t ->
    ?url:string ->
    ?range:string ->
    file_name:string ->
    string Lwt.t

  val find_all_file_info : Ezjsonm.value -> File_info.t

  val find_all_partial_file_info : Ezjsonm.value -> Partial_file_info.t

  val get_file_info : token:Token.t -> file_id:string -> File_info.t Lwt.t

  val hide_file :
    token:Token.t -> bucket_id:string -> file_name:string -> File_info.t Lwt.t

  val list_file_names :
    token:Token.t ->
    ?start_file_name:string ->
    ?max_file_count:int ->
    ?prefix:string ->
    ?delimiter:string ->
    bucket_id:string ->
    (File_info.t list * string) Lwt.t

  val list_file_versions :
    token:Token.t ->
    ?start_file_name:string ->
    ?start_file_id:string ->
    ?max_file_count:int ->
    ?prefix:string ->
    ?delimiter:string ->
    bucket_id:string ->
    (File_info.t list * string * string) Lwt.t

  val hash_string : String.t -> string

  val upload_file :
    url:Upload_url.t ->
    ?content_type:string ->
    ?file_info:(string * Ezjsonm.t) list ->
    data:char Lwt_stream.t ->
    file_name:string ->
    File_info.t Lwt.t

  val cancel_large_file : token:Token.t -> file_id:string -> File.t Lwt.t

  val start_large_file :
    token:Token.t ->
    ?content_type:string ->
    ?file_info:(string * Ezjsonm.value) list ->
    bucket_id:string ->
    file_name:string ->
    Partial_file_info.t Lwt.t

  val finish_large_file :
    token:Token.t ->
    file_id:string ->
    part_sha1_array:Cstruct.t list ->
    File_info.t Lwt.t

  val upload_part :
    url:Upload_url.t -> data:char Lwt_stream.t -> part_num:int -> Part.t Lwt.t

  val list_parts : token:Token.t -> file_id:string -> Part.t list Lwt.t

  val find_all_bucket : Ezjsonm.value -> Bucket.t

  val create_bucket :
    token:Token.t ->
    ?bucket_info:(string * Ezjsonm.value) list ->
    bucket_name:string ->
    bucket_type:[< `Private | `Public ] ->
    Bucket.t Lwt.t

  val delete_bucket : token:Token.t -> bucket_id:string -> Bucket.t Lwt.t

  val get_upload_url : token:Token.t -> bucket_id:string -> Upload_url.t Lwt.t

  val get_upload_part_url :
    token:Token.t -> file_id:string -> Upload_url.t Lwt.t

  val list_buckets : token:Token.t -> Bucket.t list Lwt.t

  val update_bucket :
    token:Token.t ->
    ?bucket_type:[< `Private | `Public ] ->
    ?bucket_info:(string * Ezjsonm.value) list ->
    ?if_revision_is:int64 ->
    bucket_id:string ->
    bucket_name:string ->
    Bucket.t Lwt.t
end
