{ callPackage, lib, mkShell, ocamlPackages }:

let
  reason = callPackage ./nix { };

in

mkShell {
  inputsFrom = [ reason ];
  buildInputs = with ocamlPackages; [
    utop
    merlin
  ];
}
