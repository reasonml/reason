{ mkShell
, ocamlPackages
, reason
, stdenv
, lib
, cacert
, curl
, git
, release-mode ? false
}:

mkShell {
  inputsFrom = [ reason ];
  buildInputs =
    with ocamlPackages; [ utop merlin odoc ]
      ++ (if release-mode then [ cacert curl dune-release git ] else [ ]);
}
