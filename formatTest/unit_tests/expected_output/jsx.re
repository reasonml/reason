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
    onKey=(updater (handleInput))
  />;

let y =
  <Routes
    path=(Routes.stateToPath (state))
    isHistorical=true
    onHashChange=(
      (_oldPath, _oldUrl, newUrl) =>
        updater (
          (
            (latestComponentBag, _) => {
              let currentActualPath =
                Routes.hashOfUri (newUrl);
              let pathFromState =
                Routes.stateToPath (
                  latestComponentBag.state
                );
              currentActualPath == pathFromState ?
                None :
                dispatchEventless (
                  (
                    State.UriNavigated (
                      currentActualPath
                    )
                  ),
                  latestComponentBag,
                  ()
                )
            }
          ),
          ()
        )
    )
  />;

let z =
  <div
    style=(
      ReactDOMRe.Style.make (
        ::width,
        ::height,
        ::color,
        ::backgroundColor,
        ::margin,
        ::padding,
        ::border,
        ::borderColor,
        ::someOtherAttribute,
        ()
      )
    )
    key=(string_of_int (1))
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
    key=(string_of_int (1))
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
    key=(string_of_int (1))
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
    key=(string_of_int (1))
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
