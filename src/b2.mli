val mk_endpoint : string -> string -> string

module V1 (C : Cohttp_lwt.S.Client) : sig
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

  type bucket_type = [ `Public | `Private ]

  module Bucket : sig
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

  val authorize_account : string -> string -> Token.t Lwt.t

  val cancel_large_file : Token.t -> string -> File.t Lwt.t

  val delete_file_version : Token.t -> string -> string -> File_version.t Lwt.t

  val get_download_authorization :
    Token.t -> string -> string -> int -> Download_authorization.t Lwt.t

  val download_file_by_id :
    ?token:Token.t -> ?url:string -> ?range:string -> string -> string Lwt.t

  val download_file_by_name :
    ?token:Token.t ->
    ?auth:Download_authorization.t ->
    ?url:string ->
    ?range:string ->
    string ->
    string Lwt.t

  val get_file_info : Token.t -> string -> File_info.t Lwt.t

  val hide_file : Token.t -> string -> string -> File_info.t Lwt.t

  val list_file_names :
    Token.t ->
    ?start_file_name:string ->
    ?max_file_count:int ->
    ?prefix:string ->
    ?delimiter:string ->
    string ->
    (File_info.t list * string) Lwt.t

  val list_file_versions :
    Token.t ->
    ?start_file_name:string ->
    ?start_file_id:string ->
    ?max_file_count:int ->
    ?prefix:string ->
    ?delimiter:string ->
    string ->
    (File_info.t list * string * string) Lwt.t

  val upload_file :
    Upload_url.t ->
    ?content_type:string ->
    ?file_info:(string * Ezjsonm.t) list ->
    char Lwt_stream.t ->
    string ->
    File_info.t Lwt.t

  val start_large_file :
    Token.t ->
    ?content_type:string ->
    ?file_info:(string * Ezjsonm.value) list ->
    string ->
    string ->
    Partial_file_info.t Lwt.t

  val finish_large_file :
    Token.t -> string -> Cstruct.t list -> File_info.t Lwt.t

  val upload_part : Upload_url.t -> char Lwt_stream.t -> int -> Part.t Lwt.t

  val list_parts : Token.t -> string -> Part.t list Lwt.t

  val create_bucket :
    Token.t ->
    ?bucket_info:(string * Ezjsonm.value) list ->
    string ->
    bucket_type ->
    Bucket.t Lwt.t

  val delete_bucket : Token.t -> string -> Bucket.t Lwt.t

  val get_upload_url : Token.t -> string -> Upload_url.t Lwt.t

  val get_upload_part_url : Token.t -> string -> Upload_url.t Lwt.t

  val list_buckets : Token.t -> Bucket.t list Lwt.t

  val update_bucket :
    Token.t ->
    ?bucket_type:bucket_type ->
    ?bucket_info:(string * Ezjsonm.value) list ->
    ?if_revision_is:Int64.t ->
    string ->
    string ->
    Bucket.t Lwt.t
end
