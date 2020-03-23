let () =
  Alcotest.run "Http-adapter test suite"
    [ Accept_test.suite_test ]
