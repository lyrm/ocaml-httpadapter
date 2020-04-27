let () =
  Alcotest.run "Http-adapter test suite"
    [ Accept_test.suite_test ;
      Method_test.suite_test ;
      Version_test.suite_test;
      Header_test.suite_test
    ]
