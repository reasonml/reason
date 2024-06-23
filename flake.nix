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
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_2;
        });
      in
      {
        packages.default = pkgs.callPackage ./nix {
          nix-filter = nix-filter.lib;
        };
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
