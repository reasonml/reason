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
            pp = osuper.pp.overrideAttrs (_: {
              doCheck = false;
            });
          });
        })
      ];
    };
in

pkgs.callPackage ./. { doCheck = true; }
