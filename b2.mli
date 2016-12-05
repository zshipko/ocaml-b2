val mk_endpoint : string -> string -> string

module V1 : sig

    module Token : sig
        type t = {
            accountId: string;
            authorizationToken: string;
            apiUrl: string;
            downloadUrl: string;
            minimumPartSize: int;
        }
    end

    module Download_authorization : sig
        type t = {
            bucketId: string;
            fileNamePrefix: string;
            authorizationToken: string;
        }
    end

    module File : sig
        type t = {
            fileId : string;
            accountId : string;
            bucketId : string;
            fileName : string;
        }
    end

    module File_version : sig
        type t = {
            fileId: string;
            fileName: string;
        }
    end

    module File_info : sig
        type t = {
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
    end

    module Partial_file_info : sig
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

    module Upload_url : sig
        type t = {
            uploadUrl: string;
            authorizationToken: string;
        }
    end

    module Part : sig
        type t = {
            fileId: string;
            partNumber: int;
            contentLength: int64;
            contentSha1: Cstruct.t;
        }
    end

    module Bucket : sig
        (* NOTE: lifecycleRules is not yet implemented *)
        type t = {
            accountId: string;
            bucketId: string;
            bucketName: string;
            bucketType: string;
            bucketInfo: Ezjsonm.value;
            revision: Int64.t;
        }
    end


    val authorize_account : string -> string -> Token.t Lwt.t
    val cancel_large_file : Token.t -> string -> File.t Lwt.t
    val delete_file_version : Token.t -> string -> string -> File_version.t Lwt.t
    val get_download_authorization : Token.t -> string -> string -> int -> Download_authorization.t Lwt.t
    val download_file_by_id : ?token:Token.t-> ?url:string -> string -> string Lwt.t
    val download_file_by_name : ?token:Token.t -> ?auth:Download_authorization.t -> ?url:string -> string -> string Lwt.t
    val get_file_info : Token.t -> string -> File_info.t Lwt.t
    val hide_file : Token.t -> string -> string -> File_info.t Lwt.t
    val list_file_names : Token.t -> ?startFileName:string -> ?maxFileCount:int -> ?prefix:string -> ?delimiter:string -> string -> (File_info.t list * string) Lwt.t
    val list_file_versions : Token.t -> ?startFileName:string -> ?startFileId:string -> ?maxFileCount:int -> ?prefix:string -> ?delimiter:string -> string -> (File_info.t list * string * string) Lwt.t
    val upload_file : Upload_url.t -> ?contentType:string -> ?fileInfo:(string * Ezjsonm.t) list -> char Lwt_stream.t -> string -> File_info.t Lwt.t
    val start_large_file : Token.t -> ?contentType:string -> ?fileInfo:(string * Ezjsonm.value) list -> string -> string -> Partial_file_info.t Lwt.t
    val finish_large_file : Token.t -> string -> Cstruct.t list -> File_info.t Lwt.t
    val upload_part : Upload_url.t -> ?contentType:string -> char Lwt_stream.t -> int -> Part.t Lwt.t
    val list_parts : Token.t -> string -> Part.t list Lwt.t
    val create_bucket : Token.t -> ?bucketInfo:(string * Ezjsonm.value) list -> string -> string -> Bucket.t Lwt.t
    val delete_bucket : Token.t -> string -> Bucket.t Lwt.t
    val get_upload_url : Token.t -> string -> Upload_url.t Lwt.t
    val get_upload_part_url : Token.t -> string -> Upload_url.t Lwt.t
    val list_buckets : Token.t -> Bucket.t list Lwt.t
    val update_bucket : Token.t -> ?bucketType:string -> ?bucketInfo:(string * Ezjsonm.value) list -> ?ifRevisionIs:Int64.t -> string -> string -> Bucket.t Lwt.t
end
