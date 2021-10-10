{ pkgs ? import ./sources.nix { } }:

let
  inherit (pkgs) stdenv lib ocamlPackages;

in

ocamlPackages.buildDunePackage {
  pname = "reason";
  version = "0.0.1-dev";

  src = lib.filterGitSource {
    src = ./..;
    dirs = [ "scripts" "src" "formatTest" ];
    files = [ "dune" "dune-project" "reason.opam" "rtop.opam" ];
  };

  useDune2 = true;

  propagatedBuildInputs = with pkgs.ocamlPackages; [
    merlin-extend
    menhir
    menhirSdk
    menhirLib
    cppo
    fix
    result
    ppx_derivers
    ppxlib
    # ocaml-migrate-parsetree
  ];

}
