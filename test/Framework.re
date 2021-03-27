let oldpwd = Unix.getenv("OLDPWD");

include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: oldpwd ++ "/snapshots",
      projectDir: "",
    });
});
