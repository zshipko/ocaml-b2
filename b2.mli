val endpoint_base : string ref

module V1 : sig

    type token = {
        accountId: string;
        authorizationToken: string;
        apiUrl: string;
        downloadUrl: string;
        minimumPartSize: int;
    }

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

    type bucket = {
        accountId: string;
        bucketId: string;
        bucketName: string;
        bucketType: string;
        bucketInfo: Ezjsonm.value;
        revision: Int64.t;
    }

    type upload_url = {
        uploadUrl: string;
        authorizationToken: string;
    }

    type download_authorization = {
        bucketId: string;
        fileNamePrefix: string;
        authorizationToken: string;
    }

    type part = {
        fileId: string;
        partNumber: int;
        contentLength: int64;
        contentSha1: Cstruct.t;
    }

    val authorize_account : string -> string -> token Lwt.t
    val cancel_large_file : token -> string -> file Lwt.t
    val delete_file_version : token -> string -> string -> file_version Lwt.t
    val get_download_authorization : token -> string -> string -> int -> download_authorization Lwt.t
    val download_file_by_id : ?token:token -> ?url:string -> string -> string Lwt.t
    val download_file_by_name : ?token:token -> ?auth:download_authorization -> ?url:string -> string -> string Lwt.t
    val get_file_info : token -> string -> file_info Lwt.t
    val hide_file : token -> string -> string -> file_info Lwt.t
    val list_file_names : token -> ?startFileName:string -> ?maxFileCount:int -> ?prefix:string -> ?delimiter:string -> string -> (file_info list * string) Lwt.t
    val list_file_versions : token -> ?startFileName:string -> ?startFileId:string -> ?maxFileCount:int -> ?prefix:string -> ?delimiter:string -> string -> (file_info list * string * string) Lwt.t
    val upload_file : upload_url -> ?contentType:string -> ?fileInfo:(string * Ezjsonm.t) list -> char Lwt_stream.t -> string -> file_info Lwt.t
    val start_large_file : token -> ?contentType:string -> ?fileInfo:(string * Ezjsonm.value) list -> string -> string -> partial_file_info Lwt.t
    val finish_large_file : token -> string -> Cstruct.t list -> file_info Lwt.t
    val upload_part : upload_url -> ?contentType:string -> char Lwt_stream.t -> int -> part Lwt.t
    val list_parts : token -> string -> part list Lwt.t
    val create_bucket : token -> ?bucketInfo:(string * Ezjsonm.value) list -> string -> string -> bucket Lwt.t
    val delete_bucket : token -> string -> bucket Lwt.t
    val get_upload_url : token -> string -> upload_url Lwt.t
    val get_upload_part_url : token -> string -> upload_url Lwt.t
    val list_buckets : token -> bucket list Lwt.t
    val update_bucket : token -> ?bucketType:string -> ?bucketInfo:(string * Ezjsonm.value) list -> ?ifRevisionIs:Int64.t -> string -> string -> bucket Lwt.t
end
