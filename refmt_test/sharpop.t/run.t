Format sharp operator
  $ refmt_impl --print-width 50 ./input.re
  foo #= bar[0];
  
  foo##bar[0] = 3;
  
  foo##bar[0]##baz[1] = 3;
  
  foo##bar[0]##baz[1];
  
  foo##bar #= bar[0];
  
  foo##bar##baz #= bar##baz[0];
  
  foo[bar + 1];
  
  foo.[bar + 1];
  
  foo.{bar + 1};
  
  foo.[bar + 1] = 1;
  
  foo.{bar + 1} = 1;
  
  foo[bar + 1] = 1;
