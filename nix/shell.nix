{ mkShell
, ocamlPackages
, reason
, stdenv
, lib
, cacert
, curl
, git
, release-mode ? false
, pkgs
}:

mkShell {
  inputsFrom = [ reason ];
  nativeBuildInputs = with ocamlPackages; [
    utop
    merlin
    # odoc
    pkgs.ocaml-ng.ocamlPackages_5_2.ocamlformat
  ];
  buildInputs =
    with ocamlPackages; (if release-mode then [ cacert curl dune-release git ] else [ ]);
}
