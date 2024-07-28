{ ocamlPackages, nix-filter, doCheck ? false }:

rec {
  reason = ocamlPackages.buildDunePackage {
    pname = "reason";
    version = "0.0.1-dev";

    src = nix-filter.filter {
      root = ./..;
      include = [
        "dune"
        "dune-project"
        "reason.opam"
        "scripts"
        "src"
        "test"
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

    src = nix-filter.filter {
      root = ./..;
      include = [
        "dune"
        "dune-project"
        "rtop.opam"
        "rtop"
        "test"
      ];
    };

    inherit doCheck;

    nativeBuildInputs = with ocamlPackages; [ cppo ];
    propagatedBuildInputs = [ reason ocamlPackages.utop ];
  };
}
