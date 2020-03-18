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

module V = Httpaf.Version

type t =
  [ `HTTP_1_0
  | `HTTP_1_1
  | `Other of string
  ]

let to_string : t -> string = function
  | `HTTP_1_0 -> "HTTP/1.0"
  | `HTTP_1_1 -> "HTTP/1.1"
  | `Other s -> s

let compare a b = String.compare (to_string a) (to_string b)

let of_string : string -> t = function
  | "HTTP/1.0" -> `HTTP_1_0
  | "HTTP/1.1" -> `HTTP_1_1
  | s -> `Other s

let parse_version = V.of_string

let to_local : t -> V.t = function
  | `HTTP_1_0 -> V.{ major = 1; minor = 0 }
  | `HTTP_1_1 -> V.{ major = 1; minor = 1 }
  | `Other s -> parse_version s

let from_local : V.t -> t = function
  | { major = 1; minor = 0 } -> `HTTP_1_0
  | { major = 1; minor = 1 } -> `HTTP_1_1
  | other -> `Other (V.to_string other)
