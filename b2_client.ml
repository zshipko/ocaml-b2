open Lwt
open Cohttp
open Cohttp_lwt_unix

let get ?ctx ?headers url =
    Client.get ?ctx ?headers (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let post ?ctx ?headers ?body url =
    Client.post ?ctx ?headers ?body (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let post_form ?ctx ?headers ~params url =
    Client.post_form ?ctx ?headers ~params (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let call ?ctx ?headers ?body meth url =
    Client.call ?ctx ?headers ?body meth (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let get_json ?ctx ?headers url =
    get ?ctx ?headers url >|= Ezjsonm.from_string

let post_json ?ctx ?headers ?json url =
    let body = match json with
    | Some j -> `String (Ezjsonm.to_string j)
    | None -> `Empty in
    post ?ctx ?headers ~body url >|= Ezjsonm.from_string

let post_form_json ?ctx ?headers ~params url =
    post_form ?ctx ?headers ~params url >|= Ezjsonm.from_string

let find_string j name =
    Ezjsonm.find j name |> Ezjsonm.get_string

let find_cstruct j name =
    find_string j name |> Cstruct.of_string

let find_int j name =
    Ezjsonm.find j name |> Ezjsonm.get_int64

let bucket_id j =
    find_string j ["bucketId"]

let file_id j =
    find_string j ["fileId"]

let file_name j =
    find_string j ["fileName"]

let account_id j =
    find_string j ["accountId"]

let bucket_name j =
    find_string j ["bucketName"]

let bucket_type j =
    find_string j ["bucketType"]

let bucket_info j =
    try Ezjsonm.find j ["bucketInfo"] with _ -> `Null

let revision j =
    find_int j ["revision"]

type error = {
    status: int;
    code: string;
    message: string;
}

exception Error_response of error
exception API_error

let handle_error x j =
    try
       x j
    with _ -> raise (try Error_response {
        status = find_int j ["status"] |> Int64.to_int;
        code = find_string j ["code"];
        message = find_string j ["message"];
    } with _ -> API_error)

