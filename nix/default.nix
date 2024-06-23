{ ocamlPackages, nix-filter }:

ocamlPackages.buildDunePackage {
  pname = "reason";
  version = "0.0.1-dev";

  src = nix-filter.filter {
    root = ./..;
    include = [
      "dune"
      "dune-project"
      "reason.opam"
      "rtop.opam"
      "scripts"
      "src"
      "test"
    ];
  };

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

}
