open Http.Header

let hstr =
  [
    ("accept", "application/xml");
    ("transfer-encoding", "chunked");
    ("accept", "text/html");
    ("content-length", "100");
  ]

let header = of_list hstr

(* Suite test for Http.Header module *)
let to_list_tests () =
  Alcotest.(check (list (pair string string)))
    "Header.to_list (Header.init ())" []
    (to_list (init ()));
  Alcotest.(check bool)
    "Header.to_list (Header.of_list h) has the same elements than h." true
    (List.fold_left
       (fun acc elt -> List.mem elt hstr && acc)
       true (to_list header))

(*let get_tests () =
  Alcotest.(check (option string))
    "Header.get (Header.add h k v) k = Some v with Header.mem h k = false"
    (Some "keep-alive")
    (get (add header "connection" "keep-alive") "connection");
  Alcotest.(check (option string))
    "Header.get (Header.add h k v) k = Some v with Header.mem h k = true"
    (Some "gzip")
    (get (add header "transfer-encoding" "gzip") "transfer-encoding")*)

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

(*
  val of_list : (name * value) list -> t

  val to_list : t -> (name * value) list

  val add : t -> name -> value -> t

  val add_unless_exists : t -> name -> value -> t

  val add_list : t -> (name * value) list -> t

  val remove : t -> name -> t

  val replace : t -> name -> value -> t

  val mem : t -> name -> bool

  val get : t -> name -> value option

  val compare : t -> t -> int

  (* Incompatible definitions :
        cohttp uses a function of type : name -> value list -> unit
        httpaf uses a function of type : name -> value -> unit
     val iter : (name -> value -> unit) -> t -> unit*)

  val fold : (name -> value -> 'a -> 'a) -> 'a -> t -> 'a

  val to_string : t -> string
*)

let suite_test =
  Alcotest.
    ( "Header module",
      [
        test_case "Header.to_list" `Quick to_list_tests;
        (*test_case "Header.add and Header.get" `Quick get_tests;*)
        test_case "Header.add and Header.get_multi" `Quick add_get_multi_tests;
        test_case "Header.add_multi and Header.get_multi" `Quick
          add_multi_get_multi_tests;
      ] )
