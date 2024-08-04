See the typed tree from ./input.re
  $ cat ./input.re | ../lib/outcometreePrinter.exe
  type t0 = T0({ t0: int, });
  type t1 = A({ x: int, }) | B | C({ c1: string, c2: string, });
  type t2(_) =
      D({ x: int, }): t2(int)
    | E({ f: int => int, }): t2(int => int)
    | F(unit): t2(unit);
