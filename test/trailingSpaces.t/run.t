Format trailing spaces
  $ refmt ./input.re
  [@reason.version 3.7];
  /* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
  
  module M =
    Something.Create({
      type resource1 = MyModule.MySubmodule.t;
      type resource2 = MyModule.MySubmodule.t;
    });
