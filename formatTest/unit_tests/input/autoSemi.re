let f = () => {
  let x = [%foo]
  let y = 1234
  let z = ldksfj;
  %ext foo;
  [%ext foo]
};

{
  %sugarOnZero
  0;
  print_string("");
  print_string("");
};

{
  let x = 0;
  %sugarOnZero
  0;
  print_string("");
  print_string("");
};
