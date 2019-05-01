# ocaml-b2

`ocaml-b2` provides OCaml bindings to the Backblaze B2 API.

## Installation

    opam pin add b2 https://github.com/zshipko/ocaml-b2.git

## API

All  methods for version 1 of the B2 API can be found in the module `B2.V1`. The
OCaml API tries to mirror the [Backblaze documentation](https://www.backblaze.com/b2/docs/) as closely as possible.

See `b2.mli` for further details.

## Usage

The following code sample prints out the name of each bucket after authenticating

```ocaml
module API = B2.V1(Cohttp_lwt_unix.Client)

let main =
    (* Get a token *)
    API.authorize_account accountId applicationKey

    (* List bucket *)
    >>= fun token -> API.list_buckets token
    >>= Lwt_list.iter (fun bucket ->
        Lwt_io.printl bucket.bucket_name)

let _ = Lwt_main.run main
```


