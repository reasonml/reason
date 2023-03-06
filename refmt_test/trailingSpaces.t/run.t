Format trailing spaces
  $ ../../src/refmt/refmt_impl.exe --print-width 50 ./input.re
  /* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
  
  module M =
    Something.Create({
      type resource1 = MyModule.MySubmodule.t;
      type resource2 = MyModule.MySubmodule.t;
    });
