{ pkgs, nix-filter }:

let
  inherit (pkgs) stdenv lib ocamlPackages;

in

ocamlPackages.buildDunePackage {
  pname = "reason";
  version = "0.0.1-dev";

  src = nix-filter.filter {
    root = ./..;
    include = [ "dune" "dune-project" "reason.opam" "rtop.opam" "scripts" "src" "test" ];
  };

  useDune2 = true;

  propagatedBuildInputs = with ocamlPackages; [
    merlin-extend
    menhir
    menhirSdk
    menhirLib
    cppo
    fix
    ppx_derivers
    ppxlib
    dune-build-info
  ];

}
