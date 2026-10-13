Format local module opens in types.

  $ refmt ./input.ml | tee formatted.re
  module F = {
    type t = int;
    type s = string;
    type r = unit;
  };
  
  type triple('a, 'b, 'c) = ('a, 'b, 'c);
  
  type paren = F.(t, s, r);
  type single = F.(t);
  type variant =
    F.(
      [
        | `X(t)
        | `Y(s)
        | `Z(r)
      ]
    );
  type nested = F.(F.(t), s);

Reason syntax is idempotent.

  $ refmt ./formatted.re > formatted-back.re
  $ diff formatted.re formatted-back.re

The formatted Reason source preserves a compiler-readable Ptyp_open.

  $ ocamlc -c -pp 'refmt --print binary' -impl formatted.re
