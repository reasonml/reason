{ ocamlVersion }:

let
  lock = builtins.fromJSON (builtins.readFile ./../flake.lock);
  pkgs =
    let
      src = fetchGit {
        url = with lock.nodes.nixpkgs.locked;"https://github.com/${owner}/${repo}";
        inherit (lock.nodes.nixpkgs.locked) rev;
        allRefs = true;
      };
    in
    import src {
      extraOverlays = [
        (self: super: {
          ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope (oself: osuper: {
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
        })
      ];
    };
in

pkgs.callPackage ./. { doCheck = true; }
