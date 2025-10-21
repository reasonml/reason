Format issue #2675 - Attributes and expressions add single space as indentation
  $ refmt ./input.re | tee formatted.re
  <Visibility hidden=loading>
   {iconPosition == `Left
      ? {
        switch (icon) {
        | Some(ico) =>
          <Spacer right=2>
            <Stack align=`Center distribute=`Center>
              ico
            </Stack>
          </Spacer>
        | None => React.null
        }
      }
      : React.null}
  </Visibility>;
  
  <ul
    className={Cn.make([
      "flex flex-row flex-wrap",
      className->Cn.unpack->Cn.ifSome(className),
    ])}>
    {switch (templateListQuery) {
     | NoData
     | Loading =>
       Belt.Array.rangeBy(0, 6, ~step=1)
       ->Utils.React.mapArray((_, _) =>
           <TemplateListItem.Loading />
         )
     | Error(e) =>
       Sentry.captureException(e);
       React.null;
     | Data(allTemplates) => React.null
     }}
  </ul>;
  
  let handleSplashScreenChange = (file: S3.file) => {
    S3.uploadFile({
      "file": file,
      "appUuid": Some(uuid),
      "workspaceUuid": None,
      "mimeType": file##type__,
    })
    |> Js.Promise.then_(url => {
         dispatch(UpdateSplashScreen(Some(url)));
         Analytics.track("Uploaded Splash Screen");
         Js.Promise.resolve();
       });
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

