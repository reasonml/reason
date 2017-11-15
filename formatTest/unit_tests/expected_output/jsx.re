let x =
  <Window
    style={
      "width": state.width,
      "height": 400,
      "paddingTop": 100,
      "paddingBottom": 100,
      "paddingLeft": 100,
      "paddingRight": 100,
      "justifyContent": CssJustifySpaceBetween,
      "flexDirection": CssFlexDirectionRow
    }
    onKey=(updater(handleInput))
  />;

let y =
  <Routes
    path=(Routes.stateToPath(state))
    isHistorical=true
    onHashChange=(
      (_oldPath, _oldUrl, newUrl) =>
        updater(
          (latestComponentBag, _) => {
            let currentActualPath =
              Routes.hashOfUri(newUrl);
            let pathFromState =
              Routes.stateToPath(
                latestComponentBag.state
              );
            currentActualPath == pathFromState ?
              None :
              dispatchEventless(
                State.UriNavigated(
                  currentActualPath
                ),
                latestComponentBag,
                ()
              )
          },
          ()
        )
    )
  />;

let z =
  <div
    style=(
      ReactDOMRe.Style.make(
        ~width,
        ~height,
        ~color,
        ~backgroundColor,
        ~margin,
        ~padding,
        ~border,
        ~borderColor,
        ~someOtherAttribute,
        ()
      )
    )
    key=(string_of_int(1))
  />;

let omega =
  <div
    aList=[
      width,
      height,
      color,
      backgroundColor,
      margin,
      padding,
      border,
      borderColor,
      someOtherAttribute
    ]
    key=(string_of_int(1))
  />;

let someArray =
  <div
    anArray=[|
      width,
      height,
      color,
      backgroundColor,
      margin,
      padding,
      border,
      borderColor,
      someOtherAttribute
    |]
    key=(string_of_int(1))
  />;

let tuples =
  <div
    aTuple=(
      width,
      height,
      color,
      backgroundColor,
      margin,
      padding,
      border,
      borderColor,
      someOtherAttribute,
      definitelyBreakere
    )
    key=(string_of_int(1))
  />;

let icon =
  <Icon
    name=(
      switch state.volume {
      | v when v < 0.1 => "sound-off"
      | v when v < 0.11 => "sound-min"
      | v when v < 0.51 => "sound-med"
      | _ => "sound-max"
      }
    )
  />;

<MessengerSharedPhotosAlbumViewPhotoReact
  ref=?(
    foo##bar === baz ?
      Some(
        foooooooooooooooooooooooo(setRefChild)
      ) :
      None
  )
  key=node##legacy_attachment_id
/>;

/* punning */
<Foo bar />;

/* punning for explicitly passed optional */
<Foo ?bar />;

/* don't pun explicitly passed optional with module identifier */
<Foo bar=?Baz.bar />;

let x = <div />;

<div asd=1 />;

foo#=<bar />;

foo#=<bar />;

let x = [|<div />|];

let x = [|
  <Button onClick=handleStaleClick />,
  <Button onClick=handleStaleClick />
|];

let z = <div />;

let z = (
  <Button onClick=handleStaleClick />,
  <Button onClick=handleStaleClick />
);
