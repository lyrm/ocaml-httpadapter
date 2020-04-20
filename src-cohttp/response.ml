(*{{{ Copyright (C) <2020> Carine Morel <carine@tarides.com>
*
* Permission to use, copy, modify, and distribute this software for any
* purpose with or without fee is hereby granted, provided that the above
* copyright notice and this permission notice appear in all copies.
*
* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
}}}*)

module R = Cohttp.Response

type t = {
  headers : Header.t;
  status : Status.t;
  version : Version.t;
  body : Body.t;
}

let from_local body : R.t -> t = function
  | { headers; status; version; _ } -> { headers; status; version; body }

let to_local : t -> R.t = function
  | { headers; status; version; body } ->
      let encoding =
        Cohttp.Header.get_transfer_encoding headers |> function
        | (Chunked | Fixed _) as encoding -> encoding
        | Unknown -> Cohttp_lwt.Body.transfer_encoding body
      in
      { headers; status; version; encoding; flush = false }

let make ?(version : Version.t = `HTTP_1_1) ?(headers = Header.init ())
    ?(body : Body.t = `Empty) (status : Status.t) =
  from_local body (R.make ~version ~status ~headers ())
