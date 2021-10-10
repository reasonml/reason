let
  pkgs = import ./nix/sources.nix { };
  inherit (pkgs) lib;
  reason = pkgs.recurseIntoAttrs (import ./nix { inherit pkgs; });

in
with pkgs;

mkShell {
  inputsFrom = [ reason ];
  buildInputs = with ocamlPackages; [
    utop
    nodejs
    merlin
  ];
}
