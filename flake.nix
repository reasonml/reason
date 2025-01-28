{
  description = "Nix Flake for ReasonML";

  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs = {
    url = "github:nix-ocaml/nix-overlays";
    inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
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
        packages = pkgs.callPackage ./nix { nix-filter = nix-filter.lib; };
      in
      {
        packages = packages // { default = packages.reason; };
        devShells = {
          default = pkgs.callPackage ./nix/shell.nix {
            reason = self.packages.${system}.default;
          };
          release = pkgs.callPackage ./nix/shell.nix {
            reason = self.packages.${system}.default;
            release-mode = true;
          };
        };
      });
}
