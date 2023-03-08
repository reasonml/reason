Format test utils
  $ refmt_impl --print-width 50 ./input.re
  /* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
  
  let printSection = s => {
    print_string("\n");
    print_string(s);
    print_string("\n---------------------\n");
  };
  
  let printLn = s => print_string(s ++ "\n");
