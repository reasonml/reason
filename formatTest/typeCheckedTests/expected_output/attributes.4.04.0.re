/* Pexp_letexception with attributes */
let () = {
  [@attribute]
  exception E;
  raise(E)
};
