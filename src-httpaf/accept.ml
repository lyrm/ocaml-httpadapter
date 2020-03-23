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

open Angstrom
open Printf

type media_range =
  | MediaType of string * string
  | AnyMediaSubtype of string
  | AnyMedia

type charset =
  | Charset of string
  | AnyCharset

type encoding =
  | Encoding of string
  | Gzip
  | Compress
  | Deflate
  | Identity
  | AnyEncoding

type language =
  | Language of string list
  | AnyLanguage

type pv =
  | T of string
  | S of string

type p = string * pv

type q = int

type 'a qlist = (q * 'a) list

(** Lexer *)
let is_space = function ' ' | '\t' -> true | _ -> false

let is_token = function
  | '\000' .. '\031'
  | '\127' | ')' | '(' | '<' | '>' | '@' | ',' | ';' | ':' | '"' | '/' | '['
  | ']' | '?' | '=' | '{' | '}' | ' ' ->
      false
  | _s -> true

let ows = skip is_space <|> return ()

let token = take_while1 is_token

let sep_by1_comma value_parser = sep_by1 (char ',') value_parser

let eval_parser parser default_value = function
  | None -> [ (1000, default_value) ]
  | Some str -> (
      match parse_string parser str with Ok v -> v | Error msg -> failwith msg )

(** Parser for header parameters like defined in rfc https://tools.ietf.org/html/rfc7231#section-5.3.2 *)
type param =
  | Q of int
  | Kv of (string * pv)

let q_of_string s = truncate (1000. *. float_of_string s)

(* More restrictive than cohttp counterpart *)
let qs = char '"' *> token <* char '"'

(* a header parameter can be :
   OWS ; OWS q=[value]
   OWS ; OWS [name]=[value]
   OWS ; OWS [name]="[value]"
*)
let param : param t =
  ows *> char ';' *> ows
  *> (* OWS ; OWS q=[value]
        OWS ; OWS [name]=[value]*)
  ( lift2
      (fun n v -> if n = "q" then Q (q_of_string v) else Kv (n, T v))
      token
      (char '=' *> token)
  <|> (* OWS ; OWS [name]="[value]" *)
  lift2 (fun n v -> Kv (n, S v)) token (char '=' *> qs) )


(* [Angstrom.many] *)
let params = many param

let rec get_q params =
  match params with [] -> 1000 | Q q :: _ -> q | _ :: r -> get_q r

let cut_params params =
  List.fold_right
    (fun param acc ->
      match (param, acc) with
      | Q q, (1000, r) -> (q, r)
      | Q _, _ ->
          failwith "There are several \"q\" parameters in the same header."
      | Kv p, (q, r) -> (q, p :: r))
    params (1000, [])

(** Parser for values of Accept header.
    Example of value: text/html,application/xml;q=0.9*)
let anymedia = string "*/*" *> return AnyMedia

let anymediasubtype =
  lift
    (fun media -> AnyMediaSubtype (String.lowercase_ascii media))
    (token <* string "/*")

let mediatype =
  lift2
    (fun media subtype ->
      MediaType (String.lowercase_ascii media, String.lowercase_ascii subtype))
    token
    (char '/' *> token)

let media_value_parser = ows *> (anymedia <|> anymediasubtype <|> mediatype)

let media_parser =
  lift2
    (fun value (q, rest) -> (q, (value, rest)))
    media_value_parser (lift cut_params params)

let media_ranges_parser : (media_range * p list) qlist t =
  sep_by1_comma media_parser

let media_ranges = eval_parser media_ranges_parser (AnyMedia, [])

(** Parser for values of Accept-charsets header.
    Example:
    Accept-charsets: iso-8859-5, unicode-1-1;q=0.8 *)
let charset_value_parser =
  ows *> (char '*' *> return AnyCharset
  <|> lift (fun t -> Charset (String.lowercase_ascii t)) token)

let charset_parser =
  lift2 (fun value q -> (q, value)) charset_value_parser (lift get_q params)

let charsets_parser = sep_by1_comma charset_parser

let charsets = eval_parser charsets_parser AnyCharset

(** Parser for values of Accept-encoding header.
    Example:
     Accept-Encoding: compress, gzip
     Accept-Encoding:
     Accept-Encoding: *
     Accept-Encoding: compress;q=0.5, gzip;q=1.0
     Accept-Encoding: gzip;q=1.0, identity; q=0.5, *;q=0 *)
let encoding_value_parser =
  ows
  *> ( char '*' *> return AnyEncoding
     <|> lift
           (fun s ->
             match String.lowercase_ascii s with
             | "gzip" -> Gzip
             | "compress" -> Compress
             | "deflate" -> Deflate
             | "identity" -> Identity
             | enc -> Encoding enc)
           token )

let encoding_parser =
  lift2 (fun value q -> (q, value)) encoding_value_parser (lift get_q params)

let encodings_parser = sep_by1_comma encoding_parser

let encodings = eval_parser encodings_parser AnyEncoding

(** Parser for values of Accept-language header.
    Example:
    Accept-Language: da, en-gb;q=0.8, en;q=0.7 *)
let language_value_parser =
  ows
  *> ( char '*' *> return AnyLanguage
     <|> lift
           (fun s ->
             Language (String.split_on_char '-' (String.lowercase_ascii s)))
           token )

let language_parser =
  lift2 (fun value q -> (q, value)) language_value_parser (lift get_q params)

let languages_parser = sep_by1_comma language_parser

let languages = eval_parser languages_parser AnyLanguage

(** Other functions (from Cohttp.Accept) *)
let rec string_of_pl = function
  | [] -> ""
  | (k, T v) :: r -> sprintf ";%s=%s%s" k v (string_of_pl r)
  | (k, S v) :: r -> sprintf ";%s=\"%s\"%s" k v (string_of_pl r)

let string_of_q = function
  | q when q < 0 -> invalid_arg (Printf.sprintf "qvalue %d must be positive" q)
  | q when q > 1000 ->
      invalid_arg (Printf.sprintf "qvalue %d must be less than 1000" q)
  | 1000 -> "1"
  | q -> Printf.sprintf "0.%03d" q

let accept_el el pl q = sprintf "%s;q=%s%s" el (string_of_q q) (string_of_pl pl)

let string_of_media_range = function
  | MediaType (t, st), pl -> accept_el (sprintf "%s/%s" t st) pl
  | AnyMediaSubtype t, pl -> accept_el (sprintf "%s/*" t) pl
  | AnyMedia, pl -> accept_el "*/*" pl

let string_of_charset = function
  | Charset c -> accept_el c []
  | AnyCharset -> accept_el "*" []

let string_of_encoding = function
  | Encoding e -> accept_el e []
  | Gzip -> accept_el "gzip" []
  | Compress -> accept_el "compress" []
  | Deflate -> accept_el "deflate" []
  | Identity -> accept_el "identity" []
  | AnyEncoding -> accept_el "*" []

let string_of_language = function
  | Language langl -> accept_el (String.concat "-" langl) []
  | AnyLanguage -> accept_el "*" []

let string_of_list s_of_el =
  let rec aux s = function
    | [ (q, el) ] -> s ^ s_of_el el q
    | [] -> s
    | (q, el) :: r -> aux (s ^ s_of_el el q ^ ",") r
  in
  aux ""

let string_of_media_ranges = string_of_list string_of_media_range

let string_of_charsets = string_of_list string_of_charset

let string_of_encodings = string_of_list string_of_encoding

let string_of_languages = string_of_list string_of_language

let qsort l =
  let compare ((i : int), _) (i', _) = compare i' i in
  List.stable_sort compare l
