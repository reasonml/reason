{ mkShell
, ocamlPackages
, reason
, cacert
, curl
, git
, release-mode ? false
}:

mkShell {
  inputsFrom = [ reason ];
  nativeBuildInputs = with ocamlPackages; [
    utop
    merlin
    odoc
    ocamlformat
  ];
  buildInputs =
    with ocamlPackages; (if release-mode then [ cacert curl dune-release git ] else [ ]);
}
