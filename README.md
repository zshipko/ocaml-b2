# ocaml-b2

`ocaml-b2` provides OCaml bindings to the Backblaze B2 API.

## Dependencies

    - lwt
    - cohttp
    - ezjsonm
    - nocrypto

## Installation

    opam pin add b2 .

## API

All  methods for version 1 of the B2 API can be found in the module `B2.V1`

See b2.mli for OCaml type signatures

## Usage

The following code sample prints out the name of each bucket after authenticating

    let main =
        let open B2.V1 in

        (* Get a token *)
        authorize_account accountId applicationKey
        >>= fun token ->
           list_buckets token
        >>= Lwt_list.iter (fun bucket ->
                Lwt_io.printl bucket.bucketName)

    let _ = Lwt_main.run main


