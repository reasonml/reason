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
            ppxlib = osuper.ppxlib.overrideAttrs (_: {
              src =
                if super.lib.versionOlder "5.4" osuper.ocaml.version then
                  osuper.ppxlib.src
                else
                  builtins.fetchurl {
                    url = "https://github.com/ocaml-ppx/ppxlib/releases/download/0.36.0/ppxlib-0.36.0.tbz";
                    sha256 = "0d54j19vi1khzmw0ffngs8xzjjq07n20q49h85hhhcf52k71pfjs";
                  };
            });
          });
        })
      ];
    };
in

pkgs.callPackage ./. { doCheck = true; }
