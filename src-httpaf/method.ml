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

module M = Httpaf.Method

type t =
  [ `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | `Other of string
  | `PATCH
  ]

let to_local : t -> M.t = function
  | `GET -> `GET
  | `HEAD -> `HEAD
  | `POST -> `POST
  | `PUT -> `PUT
  | `DELETE -> `DELETE
  | `CONNECT -> `CONNECT
  | `OPTIONS -> `OPTIONS
  | `TRACE -> `TRACE
  | `Other str -> `Other str
  | `PATCH -> `Other "patch"

let from_local : M.t -> t = function
  | `GET -> `GET
  | `HEAD -> `HEAD
  | `POST -> `POST
  | `PUT -> `PUT
  | `DELETE -> `DELETE
  | `CONNECT -> `CONNECT
  | `OPTIONS -> `OPTIONS
  | `TRACE -> `TRACE
  | `Other "patch" -> `PATCH
  | `Other str -> `Other str

let to_string meth = to_local meth |> M.to_string

let compare a b = compare (to_string a) (to_string b)
