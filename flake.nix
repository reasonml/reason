{
  description = "Nix Flake for ReasonML";

  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    let
      forAllSystems = f: nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (system:
        let
          pkgs = nixpkgs.legacyPackages.${system}.extend (self: super: {
            ocamlPackages = super.ocaml-ng.ocamlPackages_5_3;
          });
        in
        f pkgs);
    in
    {
      packages = forAllSystems (pkgs:
        let
          packages =
            pkgs.callPackage ./nix { nix-filter = nix-filter.lib; };
        in
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
