open TestFramework;
open TestUtils;

let buildArgs = filename =>
  refmtBin ++ " --print-width 50 --print re " ++ filename;

let refmTestFolders = ["idempotentTests", "typeCheckedTests", "unit_tests"];

describe("formatTest", ({describe, _}) =>
  refmTestFolders
  |> List.iter(folder =>
       describe(folder, ({test, _}) =>
         lsDir("./formatTest/" ++ folder ++ "/input")
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
     )
);
