Ensures refmt_iml.exe is in the right place
  $ ../src/refmt/refmt_impl.exe test.re --version;
  Reason 3.7.0 @ b66ed1b

  $ ocamlc -config | grep "int_size:"
  int_size: 63
