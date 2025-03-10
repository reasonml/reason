{ lib, ocamlPackages, doCheck ? false }:

rec {
  reason = ocamlPackages.buildDunePackage {
    pname = "reason";
    version = "0.0.1-dev";

    src =
      let fs = lib.fileset; in
      fs.toSource {
        root = ./..;
        fileset = fs.unions [
          ../dune-project
          ../dune
          ../reason.opam
          ../scripts
          ../src
          ../test
        ];
      };

    inherit doCheck;

    nativeBuildInputs = with ocamlPackages; [ cppo menhir ];
    propagatedBuildInputs = with ocamlPackages; [
      merlin-extend
      menhirSdk
      menhirLib
      fix
      ppx_derivers
      ppxlib
      dune-build-info
    ];

  };

  rtop = ocamlPackages.buildDunePackage {
    pname = "rtop";
    version = "0.0.1-dev";

    src =
      let fs = lib.fileset; in
      fs.toSource {
        root = ./..;
        fileset = fs.unions [
          ../dune-project
          ../dune
          ../rtop.opam
          ../scripts
          ../rtop
          ../test
        ];
      };

    inherit doCheck;

    nativeBuildInputs = with ocamlPackages; [ cppo ];
    propagatedBuildInputs = [ reason ocamlPackages.utop ];
  };
}
