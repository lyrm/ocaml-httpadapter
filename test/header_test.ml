open Http.Header

let hstr =
  [
    ("accept", "application/xml");
    ("transfer-encoding", "chunked");
    ("accept", "text/html");
    ("content-length", "100");
  ]

let header =
  List.fold_left (fun headers (n, v) -> add headers n v) (init ()) hstr

(* Suite test for Http.Header module *)

(* Tested invariants:
   - get (add h k v) k = Some v with mem h k = false
   - get (add h k v) k = Some v with mem h k = true
*)
(* Due to implementation differences between cohttp and http/af, there
   are some interesting tests missing here : if a header can have
   multiple values, the cohttp [get] function returns a string
   concatening all values separated by commas whereas the httpaf one
   returns just the last added value for this header. *)
let add_get_tests () =
  Alcotest.(check (option string))
    "Header.get (Header.add h k v) k = Some v with Header.mem h k = false"
    (Some "keep-alive")
    (get (add header "connection" "keep-alive") "connection");
  Alcotest.(check (option string))
    "Header.get (Header.add h k v) k = Some v with Header.mem h k = true"
    (Some "close")
    (get (add header "connection" "close") "connection")

(* Tested invariants:
   - mem (init ()) k = false
   - mem (add (init ()) k v) k = true
*)
let mem_tests () =
  Alcotest.(check bool)
    "Header.mem (Header.init ()) k = false" false
    (mem (init ()) "accept");
  Alcotest.(check bool)
    "Header.mem (Header.add (Header.init ()) k v) k = true" true
    (mem (add (init ()) "accept" "text/*") "accept")

(* Tested invariants:
   - get (add_unless_exists h k v) k = v if mem h k = true
   - get (add_unless_exists h k v) k = v' if get h k = v'
*)
let add_unless_exists_get_tests () =
  Alcotest.(check (option string))
    "Header.get (Header.add_unless_exists h k v) k = v if Header.mem h k = true"
    (Some "close")
    (get (add_unless_exists header "connection" "close") "connection");
  Alcotest.(check (option string))
    "Header.get (Header.add_unless_exists h k v) k = v' if Header.get h k = v'"
    (Some "chunked")
    (get
       (add_unless_exists header "transfer-encoding" "gzip")
       "transfer-encoding")

(* Tested invariants:
   - get_multi (add h k n) k = [n] if mem h k = None
   - get_multi (add h k n) k = vs@[n] if get_multi h k = vs
*)
let add_get_multi_tests () =
  Alcotest.(check (list string))
    "Header.get_multi (Header.add h k n) k = [n] if Header.mem h k = None"
    [ "keep-alive" ]
    (get_multi (add header "connection" "keep-alive") "connection");
  Alcotest.(check (list string))
    "Header.get_multi (Header.add h k n) k = vs@[n] if Header.get_multi h k = \
     vs"
    [ "application/xml"; "text/html"; "image/*" ]
    (get_multi (add header "accept" "image/*") "accept")

(* Tested invariants:
   - get_multi (add_multi h k []) k = vs with get_multi h k = vs
   - get_multi (add_multi h k vs) k = vs with mem h k = false
   - get_multi (add_multi h k vs) k = vs'@vs with get_multi h k = vs'
*)
let add_multi_get_multi_tests () =
  Alcotest.(check (list string))
    "Header.get_multi (Header.add_multi h k []) k' = vs with Header.get_multi \
     h k = vs"
    [ "application/xml"; "text/html" ]
    (get_multi (add_multi header "a" []) "accept");
  Alcotest.(check (list string))
    "Header.get_multi (Header.add_multi h k vs) k = vs with Header.mem h k = \
     false"
    [ "keep-alive"; "close" ]
    (get_multi
       (add_multi header "connection" [ "keep-alive"; "close" ])
       "connection");
  Alcotest.(check (list string))
    "Header.get_multi (Header.add_multi h k vs) k = vs'@vs with \
     Header.get_multi h k = vs' "
    [ "application/xml"; "text/html"; "image/*"; "application/xhtml" ]
    (get_multi
       (add_multi header "accept" [ "image/*"; "application/xhtml" ])
       "accept")

(* Tested invariants
  - get_multi (add_list h [k, v; k', v']) k'' = [] if mem h k'' = false
  - get_multi (add_list h [k, v; k', v']) k = vs@[v] if get_multi h k = vs
  - get_multi (add_list h [k, v; k, v']) k = vs@[v;v'] if get_multi h k = vs
*)
let add_list_get_multi_tests () =
  Alcotest.(check (list string))
    "Header.get_multi (Header.add_list h [k, v; k', v']) k'' = [] if \
     Header.mem h k'' = false"
    []
    (get_multi
       (add_list (init ())
          [ ("transfer-encoding", "chunked"); ("connection", "close") ])
       "accept");
  Alcotest.(check (list string))
    "Header.get_multi (Header.add_list h [k, v; k', v']) k = vs@[v] if \
     Header.get_multi h k = vs "
    ["application/xml"; "text/html"; "image/*"]
    (get_multi
       (add_list header [ ("accept", "image/*"); ("connection", "close") ])
       "accept");
  Alcotest.(check (list string))
    "Header.get_multi (Header.add_list h [k, v; k, v']) k = vs@[v;v'] if \
     Header.get_multi h k = vs "
    ["application/xml"; "text/html"; "image/*"; "application/*"]
    (get_multi
       (add_list header [ ("accept", "image/*"); ("accept", "application/*") ])
       "accept")

(* Tested invariants
   - to_list (init ())
   - to_list (of_list h) has the same elements than h
*)
(* The overall order is not the same since cohttp used a map structure
   for headers where http/af use an associative list.
  *)
let to_list_tests () =
  Alcotest.(check (list (pair string string)))
    "Header.to_list (Header.init ())" []
    (to_list (init ()));
  Alcotest.(check bool)
    "Header.to_list (Header.of_list h) has the same elements than h." true
    (List.fold_left
       (fun acc elt -> List.mem elt hstr && acc)
       true (to_list header))

(* Tested invariants:
   - mem (remove h n) n = false if get_multi h n = []
   - mem (remove h n) n = false if get_multi h n = [v]
   - mem (remove h n) n = false if get_multi h n = vs
   - mem (remove h n) n' = true if mem h n' = true
*)
let remove_mem_tests () =
  Alcotest.(check bool)
    "Header.mem (Header.remove h n) n = false if Header.get_multi h n = []"
    false
    (mem (remove header "connection") "connection");
  Alcotest.(check bool)
    "Header.mem (Header.remove h n) n = false if Header.get_multi h n = [v]"
    false
    (mem (remove header "transfer-encoding") "transfer-encoding");
  Alcotest.(check bool)
    "Header.mem (Header.remove h n) n = false if Header.get_multi h n = vs"
    false
    (mem (remove header "accept") "accept");
  Alcotest.(check bool)
    "Header.mem (Header.remove h n) n' = true if Header.mem h n' = true"
    true
    (mem (remove header "accept") "transfer-encoding")

(* Tested invariants:
   - get_multi (replace h k v) k = [] if [get_multi h v] = []
   - get_multi (replace h k v) k = [v] if [get_multi h v] = _ :: _
*)
let replace_tests () =
  Alcotest.(check (list string))
    "Header.get_multi (Header.replace "
    []
    (get_multi (replace header "connection" "close" ) "connection");
  Alcotest.(check (list string))
    ""
    ["gzip"]
    (get_multi (replace header "transfer-encoding" "gzip" ) "transfer-encoding");
  Alcotest.(check (list string))
    "truc"
    ["image/*"]
    (get_multi (replace header "accept" "image/*" ) "accept")


(* Tested invariants
   - compare (to_list l) (add_list (init ()) l) = 0

   TODO : Add more tests for this function !
*)
let compare_tests () =
  Alcotest.(check int)
    "Header.compare (Header.to_list l) (Header.add_list (Header.init ()) l) = 0"
    0
    (compare (of_list hstr) (add_list (init ()) hstr))

(*
   TODO :
  val of_list : (name * value) list -> t
  val fold : (name -> value -> 'a -> 'a) -> 'a -> t -> 'a
  val to_string : t -> string

  (* Incompatible definitions :
        cohttp uses a function of type : name -> value list -> unit
        httpaf uses a function of type : name -> value -> unit
     val iter : (name -> value -> unit) -> t -> unit*)
*)

let suite_test =
  Alcotest.
    ( "Header module",
      [
        test_case "Header.mem" `Quick mem_tests;
        test_case "Header.add and Header.get" `Quick add_get_tests;
        test_case "Header.add_unless_exists and Header.get" `Quick
          add_unless_exists_get_tests;
        test_case "Header.add and Header.get_multi" `Quick add_get_multi_tests;
        test_case "Header.add_multi and Header.get_multi" `Quick
          add_multi_get_multi_tests;
        test_case "Header.add_list and Header.get_multi" `Quick
          add_list_get_multi_tests;
        test_case "Header.to_list" `Quick to_list_tests;
        test_case "Header.remove" `Quick remove_mem_tests;
        test_case "Header.replace" `Quick replace_tests;
        test_case "Header.compare and various invariants" `Quick compare_tests
      ] )
