open Http.Accept

(* Suite test for Http.Accept module *)

module Alcotest = struct
  include Alcotest

  let fmt_p = Fmt.pair ~sep:(Fmt.any "=") Fmt.string Fmt.string

  let fmt_qlist fmt fmt_elt qlist =
    Fmt.pf fmt "%a"
      (Fmt.list ~sep:Fmt.semi (Fmt.pair ~sep:Fmt.comma Fmt.int fmt_elt))
      qlist

  let fmt_media_range fmt media_range =
    match media_range with
    | MediaType (t, st) ->
        Fmt.pf fmt "Mediatype %a"
          (Fmt.parens (Fmt.pair ~sep:Fmt.comma Fmt.string Fmt.string))
          (t, st)
    | AnyMediaSubtype t -> Fmt.pf fmt "AnyMediaSubtype %s" t
    | AnyMedia -> Fmt.pf fmt "AnyMedia"

  let fmt_media_ranges fmt media_ranges =
    fmt_qlist fmt
      (Fmt.pair ~sep:Fmt.comma fmt_media_range
         (Fmt.brackets (Fmt.list ~sep:Fmt.semi fmt_p)))
      media_ranges

  let fmt_charset fmt charset =
    ( match charset with
    | Charset str -> "Charset " ^ str
    | AnyCharset -> "Anycharset" )
    |> Fmt.pf fmt "%s"

  let fmt_charsets fmt charsets = fmt_qlist fmt fmt_charset charsets

  let fmt_language fmt language =
    match language with
    | AnyLanguage -> Fmt.pf fmt "%s" "AnyLanguage"
    | Language langs ->
        Fmt.pf fmt "%a" (Fmt.list ~sep:(Fmt.any "-") Fmt.string) langs

  let fmt_languages fmt languages = fmt_qlist fmt fmt_language languages

  let fmt_encoding fmt encoding =
    ( match encoding with
    | Encoding enc -> "Encoding " ^ enc
    | Gzip -> "gzip"
    | Compress -> "compress"
    | Deflate -> "deflate"
    | Identity -> "deflate"
    | AnyEncoding -> "AnyEncoding" )
    |> Fmt.pf fmt "%s"

  let fmt_encodings fmt encodings = fmt_qlist fmt fmt_encoding encodings

  let media_ranges = Alcotest.testable fmt_media_ranges ( = )

  let charsets = Alcotest.testable fmt_charsets ( = )

  let encodings = Alcotest.testable fmt_encodings ( = )

  let languages = Alcotest.testable fmt_languages ( = )
end

(* Tests for [Accept.media_ranges] function *)
let media_ranges_tests () =
  Alcotest.(check media_ranges)
    "No value"
    [ (1000, (AnyMedia, [])) ]
    (media_ranges None);
  Alcotest.(check media_ranges)
    "*/* with no parameter"
    [ (1000, (AnyMedia, [])) ]
    (media_ranges (Some "*/*"));
  Alcotest.(check media_ranges)
    "*/* with q parameter"
    [ (700, (AnyMedia, [])) ]
    (media_ranges (Some "*/*;q=0.7"));
  Alcotest.(check media_ranges)
    "text/* with no parameter"
    [ (1000, (AnyMediaSubtype "text", [])) ]
    (media_ranges (Some "text/*"));
  Alcotest.(check media_ranges)
    "text/* with q parameter"
    [ (250, (AnyMediaSubtype "text", [])) ]
    (media_ranges (Some "text/*;q=0.25"));
  Alcotest.(check media_ranges)
    "text/html with no parameter"
    [ (1000, (MediaType ("text", "html"), [])) ]
    (media_ranges (Some "text/html"));
  Alcotest.(check media_ranges)
    "text/html with q parameter"
    [ (666, (MediaType ("text", "html"), [])) ]
    (media_ranges (Some "text/html;q=0.666"));
  Alcotest.(check media_ranges)
    "text/html with several parameters"
    [ (666, (MediaType ("text", "html"), [ ("a", "42"); ("b", "42") ])) ]
    (media_ranges (Some "text/html;q=0.666;a=42;b=\"42\""));
  Alcotest.(check media_ranges)
    "Several values with parameters"
    [
      (1000, (MediaType ("text", "html"), []));
      (900, (MediaType ("application", "xml"), [ ("a", "42") ]));
    ]
    (media_ranges (Some "text/html,application/xml;q=0.9;a=42"));
  Alcotest.(check media_ranges)
    "Several values with allowed spaces"
    [
      (1000, (MediaType ("text", "html"), []));
      (900, (MediaType ("application", "xml"), [ ("a", "42") ]));
    ]
    (media_ranges (Some "text/html, application/xml; q=0.9; a=42"))

(* TODO : add failure tests *)

(* Tests for [Accept.charsets] function *)
let charsets_tests () =
  Alcotest.(check charsets) "No value" [ (1000, AnyCharset) ] (charsets None);
  Alcotest.(check charsets)
    "* with no parameter" [ (1000, AnyCharset) ] (charsets (Some "*"));
  Alcotest.(check charsets)
    "* with q parameter" [ (700, AnyCharset) ]
    (charsets (Some "*;q=0.7"));
  Alcotest.(check charsets)
    "unicode-1-1 with no parameter"
    [ (1000, Charset "unicode-1-1") ]
    (charsets (Some "unicode-1-1"));
  Alcotest.(check charsets)
    "unicode-1-1 with q parameter"
    [ (800, Charset "unicode-1-1") ]
    (charsets (Some "unicode-1-1;q=0.800"));
  Alcotest.(check charsets)
    "Several values with parameters"
    [ (1000, Charset "unicode-1-1"); (900, Charset "iso-8859-5") ]
    (charsets (Some "unicode-1-1,iso-8859-5;q=0.9"));
  Alcotest.(check charsets)
    "Several values with allowed spaces"
    [ (1000, Charset "unicode-1-1"); (900, Charset "iso-8859-5") ]
    (charsets (Some "unicode-1-1, iso-8859-5; q=0.9"))

(* Tests for [Accept.encodings] function *)
let encodings_tests () =
  Alcotest.(check encodings) "No value" [ (1000, AnyEncoding) ] (encodings None);
  Alcotest.(check encodings)
    "* with no parameter" [ (1000, AnyEncoding) ] (encodings (Some "*"));
  Alcotest.(check encodings)
    "* with q parameter" [ (700, AnyEncoding) ]
    (encodings (Some "*;q=0.7"));
  Alcotest.(check encodings)
    "gzip with no parameter" [ (1000, Gzip) ] (encodings (Some "gzip"));
  Alcotest.(check encodings)
    "compress with q parameter" [ (800, Compress) ]
    (encodings (Some "compress;q=0.800"));
  Alcotest.(check encodings)
    "Several values with parameters"
    [ (1000, Deflate); (900, Identity) ]
    (encodings (Some "deflate,identity;q=0.9"));
  Alcotest.(check encodings)
    "Several values with allowed spaces"
    [ (1000, Encoding "some_value"); (900, Gzip) ]
    (encodings (Some "some_value, gzip; q=0.9"))

(* Tests for [Accept.languages] function *)
let languages_tests () =
  Alcotest.(check languages) "No value" [ (1000, AnyLanguage) ] (languages None);
  Alcotest.(check languages)
    "* with no parameter" [ (1000, AnyLanguage) ] (languages (Some "*"));
  Alcotest.(check languages)
    "* with q parameter" [ (700, AnyLanguage) ]
    (languages (Some "*;q=0.7"));
  Alcotest.(check languages)
    "fr with no parameter"
    [ (1000, Language [ "fr" ]) ]
    (languages (Some "fr"));
  Alcotest.(check languages)
    "en-uk with q parameter"
    [ (800, Language [ "en"; "uk" ]) ]
    (languages (Some "en-uk;q=0.800"));
  Alcotest.(check languages)
    "Several values with parameters"
    [ (1000, Language [ "fr" ]); (900, Language [ "en"; "uk" ]) ]
    (languages (Some "fr,en-uk;q=0.9"));
  Alcotest.(check languages)
    "Several values with allowed spaces"
    [ (1000, Language [ "fr" ]); (900, Language [ "en"; "uk" ]) ]
    (languages (Some "fr, en-uk; q=0.9"))

(* Tests for all printer functions in the Accept module
   (string_of_media_range, string_of_encoding, etc.. *)
let printer_tests () =
  Alcotest.(check string)
    "Accept.string_of_media_range" "*/*;q=1;n1=v1;n2=\"v2\""
    (string_of_media_range (AnyMedia, [ ("n1", "v1"); ("n2", "\"v2\"") ]) 1000);
  Alcotest.(check string)
    "Accept.string_of_media_ranges"
    "*/*;q=1;n1=v1;n2=\"v2\",text/*;q=0.900,application/xml;q=0.666"
    (string_of_media_ranges
       [
         (1000, (AnyMedia, [ ("n1", "v1"); ("n2", "\"v2\"") ]));
         (900, (AnyMediaSubtype "text", []));
         (666, (MediaType ("application", "xml"), []));
       ]);
  Alcotest.(check string)
    "Accept.string_of_charset" "iso-8859-5;q=0.900"
    (string_of_charset (Charset "iso-8859-5") 900);
  Alcotest.(check string)
    "Accept.string_of_charsets" "iso-8859-5;q=1,unicode-1-1;q=0.500"
    (string_of_charsets
       [ (1000, Charset "iso-8859-5"); (500, Charset "unicode-1-1") ]);
  Alcotest.(check string)
    "Accept.string_of_language" "en-uk;q=0.900"
    (string_of_language (Language [ "en"; "uk" ]) 900);
  Alcotest.(check string)
    "Accept.string_of_languages" "en-uk;q=1,fr;q=0.500"
    (string_of_languages
       [ (1000, Language [ "en"; "uk" ]); (500, Language [ "fr" ]) ]);
  Alcotest.(check string)
    "Accept.string_of_encoding" "gzip;q=0.900"
    (string_of_encoding Gzip 900);
  Alcotest.(check string)
    "Accept.string_of_encodings" "deflate;q=1,compress;q=1,identity;q=0.500"
    (string_of_encodings [ (1000, Deflate); (1000, Compress); (500, Identity) ])

let suite_test =
  Alcotest.
    ( "Accept module",
      [
        test_case "Accept.media_ranges" `Quick media_ranges_tests;
        test_case "Accept.charsets" `Quick charsets_tests;
        test_case "Accept.encodings" `Quick encodings_tests;
        test_case "Accept.languages" `Quick languages_tests;
        test_case "Accept.string_of_*" `Quick printer_tests;
      ] )
