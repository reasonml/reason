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
    onKey=(updater handleInput)
  />;