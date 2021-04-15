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

module B = Httpaf.Body

type t =
  [ `Empty | `String of string | `Strings of string list | `Stream of stream ]

and stream = string Lwt_stream.t

let of_string s = `String s

let to_string : t -> string Lwt.t =
  Lwt.(
    function
    | `Empty -> return ""
    | `String s -> return s
    | `Strings sl -> return (String.concat "" sl)
    | `Stream s ->
        let b = Buffer.create 1024 in
        Lwt_stream.iter (Buffer.add_string b) s >>= fun () ->
        return (Buffer.contents b))

let of_string_list s = `Strings s
