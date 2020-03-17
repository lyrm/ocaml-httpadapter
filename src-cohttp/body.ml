module B = Cohttp.Body

type t =
  [ `Empty
  | `String of string
  | `Strings of string list
  | `Stream of stream
  ]

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
