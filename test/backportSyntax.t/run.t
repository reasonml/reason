  $ refmt ./input.re --print binary > ./output.bin

  $ ocamlc -impl ./output.bin -o ./out

  $ ./out
  Success
