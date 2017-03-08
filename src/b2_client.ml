open Lwt
open Cohttp
open Cohttp_lwt_unix

let get ?ctx headers url =
    Client.get ?ctx ~headers (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let post ?ctx ?body:(body=`Empty) headers url =
    Cohttp_lwt_body.length body
    >>= fun (len, body) ->
        let headers = Cohttp.Header.replace headers "Content-Length" (Int64.to_string len) in
        Client.post ?ctx ~headers ~body (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let post_form ?ctx ?params:(params=[]) headers url =
    let len = Uri.encoded_of_query params |> String.length in
    let headers = Cohttp.Header.replace headers "Content-Length" (string_of_int len) in
    Client.post_form ?ctx ~headers ~params (Uri.of_string url) >>= fun (res, body) ->
        Cohttp_lwt_body.to_string body

let post_json ?ctx ?json headers url =
    let body, len = match json with
    | Some j ->
        let s = Ezjsonm.to_string j in
        `String s, String.length s
    | None -> `Empty, 0 in
    post ?ctx ~body headers url >|= Ezjsonm.from_string

let get_json ?ctx headers url =
    get ?ctx headers url >|= Ezjsonm.from_string

let post_form_json ?ctx ~params headers url =
    post_form ?ctx ~params headers url >|= Ezjsonm.from_string

let find_default fn j name d =
    try
        fn j name
    with _ -> d


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
    with exc -> raise (try Error_response {
        status = find_int j ["status"] |> Int64.to_int;
        code = find_string j ["code"];
        message = find_string j ["message"];
    } with _ -> raise exc)

