open Http.Version

(* Suite test for Http.Version module *)
module Alcotest = struct
  include Alcotest

  let fmt_version fmt = function
    | `HTTP_1_0 -> Fmt.pf fmt "HTTP/1.0"
    | `HTTP_1_1 -> Fmt.pf fmt "HTTP/1.1"
    | `Other s -> Fmt.pf fmt "%s" s

  let version = Alcotest.testable fmt_version ( = )
end

(* Tests for Versions.compare functions *)
let versions =
  [
    `HTTP_1_0;
    `HTTP_1_1;
    `Other "HTTP/2.0";
    `Other "HTTP/2.1";
    `Other "HTTP/3.0";
  ]

let nb_versions = List.length versions

let compare_tests () =
  (* All comparisons between twice the same versions. *)
  let tests_true = List.map (fun v -> compare v v) versions in

  (* All comparisons between differents versions. *)
  let tests_false =
    let c1 = ref (-1) in
    List.fold_left
      (fun acc v1 ->
        let c2 = ref (-1) in
        c1 := !c1 + 1;
        let v =
          List.fold_left
            (fun v' v2 ->
              c2 := !c2 + 1;
              if !c1 = !c2 then v' else v' + abs (compare v1 v2))
            0 versions
        in
        v + acc)
      0 versions
  in

  Alcotest.(check (list int))
    "Version.compare"
    (List.init nb_versions (fun _ -> 0))
    tests_true;
  Alcotest.(check bool)
    "Version.compare" true
    (tests_false = nb_versions * (nb_versions - 1))

let of_string_tests () =
  Alcotest.(check version)
    "Version.of_string \"HTTP/1.0\"" `HTTP_1_0 (of_string "HTTP/1.0");
  Alcotest.(check version)
    "Version.of_string \"HTTP/1.1\"" `HTTP_1_1 (of_string "HTTP/1.1");
  Alcotest.(check version)
    "Version.of_string \"HTTP/2.1\"" (`Other "HTTP/2.1") (of_string "HTTP/2.1")

let suite_test =
  Alcotest.
    ( "Version module",
      [
        test_case "Version.compare" `Quick compare_tests;
        test_case "Version.of_string" `Quick of_string_tests;
      ] )
