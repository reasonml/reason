open TestFramework;
open TestUtils;

let buildArgs = filename =>
  refmtBin ++ " --print-width 50 --print re " ++ filename;

let refmTestFolders = ["idempotentTests", "typeCheckedTests", "unit_tests"];

describe("formatTest", ({describe, _}) => {
  refmTestFolders
  |> List.iter(folder =>
       describe(folder, ({test, _}) =>
         lsDir("./formatTest/" ++ folder ++ "/input")
         |> List.iter(filename =>
              test(
                filename,
                ({expect}) => {
                  let (stdOut, stdErr) = syscall(buildArgs(filename));
                  expect.string(stdOut).toMatchSnapshot();
                  expect.string(stdErr).toBeEmpty();
                },
              )
            )
       )
     );

  describe("errorTests", ({test, _}) =>
    lsDir("./formatTest/errorTests/input")
    |> List.iter(filename =>
         test(
           filename,
           ({expect}) => {
             let (stdOut, stdErr) = syscall(buildArgs(filename));
             expect.string(stdErr).toMatchSnapshot();
             expect.string(stdOut).toBeEmpty();
           },
         )
       )
  );
});
