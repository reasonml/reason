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
          ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}";
        })
      ];
    };
  nix-filter = import (fetchGit {
    url = with lock.nodes.nix-filter.locked; "https://github.com/${owner}/${repo}";
    inherit (lock.nodes.nix-filter.locked) rev;
    # inherit (lock.nodes.nixpkgs.original) ref;
    allRefs = true;
  });

  inherit (pkgs) callPackage;

in
callPackage ./. {
  doCheck = true;
  inherit nix-filter;
}
