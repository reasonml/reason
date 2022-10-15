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
  buildInputs = with ocamlPackages; [
    utop
    merlin
  ]
  ++ (if release-mode then [
    cacert
    curl
    ocamlPackages.dune-release
    git
  ] else [ ])
  ;

}
