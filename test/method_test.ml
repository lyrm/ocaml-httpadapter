open Http.Method

(* Suite test for Http.Method module *)

(* Tests for Method.compare functions *)
let meth =
  [
    `GET;
    `HEAD;
    `POST;
    `PUT;
    `DELETE;
    `CONNECT;
    `OPTIONS;
    `TRACE;
    `Other "Some meth";
    `Other "another meth";
    `PATCH;
  ]

let meth_nb = List.length meth

let compare_tests () =
  (* All comparisons between twice the same method. *)
  let tests_true = List.map (fun meth -> compare meth meth) meth in

  (* All comparisons between different methods
     (meth_nb * (meth_nb-1) comparisons) *)
  let tests_false =
    let c1 = ref (-1) in
    List.fold_left
      (fun acc meth1 ->
        let c2 = ref (-1) in
        c1 := !c1 + 1;
        let v =
          List.fold_left
            (fun v' meth2 ->
              c2 := !c2 + 1;
              if !c1 = !c2 then v' else v' + abs (compare meth1 meth2))
            0 meth
        in
        v + acc)
      0 meth
  in
  Alcotest.(check (list int))
    "Method.compare"
    (List.init meth_nb (fun _ -> 0))
    tests_true;
  Alcotest.(check bool)
    "Method.compare" true
    (tests_false = meth_nb * (meth_nb - 1))



let suite_test =
  Alcotest.("Method module", [ test_case "Method.compare" `Quick compare_tests ])
