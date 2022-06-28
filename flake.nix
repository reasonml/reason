{
  description = "Nix Flake for ReasonML";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:anmonteiro/nix-overlays";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_00;
        });
      in
      rec {
        packages.default = pkgs.callPackage ./nix { };
        devShells.default = pkgs.callPackage ./shell.nix { };
      });
}
