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
                  rev = "3374fe83926ea192ceccc9977032bff72ecaf2f7";
                  hash = "sha256-KfuA31hmFHpPkp7lq7lH6jaQyLiqBd/UnY5+ctntmF0=";
                };
              });
              pp = osuper.pp.overrideAttrs (_: {
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
