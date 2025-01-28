{
  description = "Nix Flake for ReasonML";

  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = { self, nixpkgs }:
    let
      forAllSystems = f: nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (system:
        let
          pkgs = nixpkgs.legacyPackages.${system}.extend (self: super: {
            ocamlPackages = super.ocaml-ng.ocamlPackages_5_3.overrideScope (oself: osuper: {
              ppxlib = osuper.ppxlib.overrideAttrs (_: {
                src = super.fetchFromGitHub {
                  owner = "ocaml-ppx";
                  repo = "ppxlib";
                  rev = "30ebdf9c31b14b8efb23b11575e97c6a43aa4554";
                  hash = "sha256-NPerzuytyeKhssiNOA3uufy3thbWnRCkDLXR2yfR//k=";
                };
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
