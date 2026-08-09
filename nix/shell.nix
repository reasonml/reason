{
  mkShell,
  ocamlPackages,
  reason,
  cacert,
  curl,
  git,
  dune-release ? ocamlPackages.dune-release,
  release-mode ? false,
}:

mkShell {
  inputsFrom = [ reason ];
  nativeBuildInputs = with ocamlPackages; [
    cinaps
    utop
    merlin
    # odoc
    ocamlformat
  ];
  buildInputs =
    with ocamlPackages;
    (
      if release-mode then
        [
          cacert
          curl
          dune-release
          git
        ]
      else
        [ ]
    );
}
