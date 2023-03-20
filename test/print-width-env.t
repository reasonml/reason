Create a file with a long line
  $ cat >test.re <<EOF
  > let initialState = uiStateFromValidated(~ownership=RemoteData.NotAsked, ~limits=initialLimits, SiteAuditSettings.default);
  > EOF

Set the print width to 120 characters via env "REFMT_PRINT_WIDTH"
  $ REFMT_PRINT_WIDTH=120 refmt test.re
  let initialState =
    uiStateFromValidated(~ownership=RemoteData.NotAsked, ~limits=initialLimits, SiteAuditSettings.default);

Set the print width to 80 characters via env "REFMT_PRINT_WIDTH"
  $ REFMT_PRINT_WIDTH=80 refmt test.re
  let initialState =
    uiStateFromValidated(
      ~ownership=RemoteData.NotAsked,
      ~limits=initialLimits,
      SiteAuditSettings.default,
    );
