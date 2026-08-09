{ ocamlVersion }:

let
  lock = builtins.fromJSON (builtins.readFile ./../flake.lock);
  pkgs =
    let
      src = fetchGit {
        url = with lock.nodes.nixpkgs.locked; "https://github.com/${owner}/${repo}";
        inherit (lock.nodes.nixpkgs.locked) rev;
        allRefs = true;
      };
      basePkgs = import src { };
    in
    basePkgs.extend (
      _self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}";
      }
    );
in

pkgs.callPackage ./. { doCheck = true; }
