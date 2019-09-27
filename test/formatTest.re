open TestFramework;
open TestUtils;

let buildArgs = filename =>
  refmtBin ++ " --print-width 50 --print re " ++ filename;

describe("formatTest", ({describe, _}) =>
  describe("unit_tests", ({test, _}) =>
    lsDir("./formatTest/unit_tests/input")
    |> List.iter(filename =>
         test(
           filename,
           ({expect}) => {
             let result = syscall(buildArgs(filename));
             expect.string(result).toMatchSnapshot();
           },
         )
       )
  )
);
