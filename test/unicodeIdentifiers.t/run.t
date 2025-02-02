Format file with unicode identifiers

  $ refmt ./input.re
  type saison =
    | Hiver
    | Été
    | Printemps
    | Automne;
  let x = Été;
