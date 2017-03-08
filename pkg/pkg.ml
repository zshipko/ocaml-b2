#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"

open Topkg

let () =
    Pkg.describe "b2" @@ fun c ->
        Ok [
            Pkg.mllib ~api:["B2"] "src/b2.mllib";
        ]
