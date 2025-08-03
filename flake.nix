{
  description = "Nix Flake for ReasonML";

  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = { self, nixpkgs }:
    let
      forAllSystems = f: nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (system:
        let
          pkgs = nixpkgs.legacyPackages.${system}.extend (self: super: {
            ocamlPackages = super.ocaml-ng.ocamlPackages_5_4.overrideScope (oself: osuper: {
              ppxlib =
                osuper.ppxlib.overrideAttrs (_: {
                  src =
                    if super.lib.versionOlder "5.4" osuper.ocaml.version then
                      osuper.ppxlib.src
                    else
                      builtins.fetchurl
                        {
                          url = "https://github.com/ocaml-ppx/ppxlib/releases/download/0.36.1/ppxlib-0.36.1.tbz";
                          sha256 = "1czgf474himz3wj3qqmy8zrsn0m40yj2z9imlhb491d1xv1vllk1";
                        };
                });

              pp = osuper.pp.overrideAttrs (_: {
                doCheck = false;
              });

              dune-release = osuper.dune-release.overrideAttrs (_: {
                buildInputs = with oself; [
                  curly
                  fmt
                  cmdliner
                  re
                  opam-format
                  opam-state
                  opam-core
                  rresult
                  logs
                  bos
                  yojson
                  astring
                  fpath
                ];
                doCheck = false;

              });
            });
          });
        in
        f pkgs);
    in
    {
      packages = forAllSystems (pkgs:
        let packages = pkgs.callPackage ./nix { }; in
        { inherit packages; default = packages.reason; }
      );

      devShells = forAllSystems (pkgs: {
        default = pkgs.callPackage ./nix/shell.nix {
          reason = self.packages.${pkgs.system}.default;
        };
        release = pkgs.callPackage ./nix/shell.nix {
          reason = self.packages.${pkgs.system}.default;
          release-mode = true;
        };
      });
    };
}
