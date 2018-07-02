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
      "flexDirection": CssFlexDirectionRow,
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
                latestComponentBag.state,
              );
            currentActualPath == pathFromState ?
              None :
              dispatchEventless(
                State.UriNavigated(
                  currentActualPath,
                ),
                latestComponentBag,
                (),
              );
          },
          (),
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
        (),
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
      someOtherAttribute,
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
      someOtherAttribute,
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
      definitelyBreakere,
    )
    key=(string_of_int(1))
  />;

let icon =
  <Icon
    name=(
      switch (state.volume) {
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
        foooooooooooooooooooooooo(setRefChild),
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
  <Button onClick=handleStaleClick />,
|];

let z = <div />;

let z = (
  <Button onClick=handleStaleClick />,
  <Button onClick=handleStaleClick />,
);

let y = [<div />, <div />];

let y = [
  <Button onClick=handleStaleClick />,
  <Button onClick=handleStaleClick />,
];

<Description term={<Text text="Age" />}>
  child
</Description>;
<Description
  term=(
    Text.createElement(
      ~text="Age",
      ~children=[],
      (),
    )
  )>
  child
</Description>;
<Description
  term=(
    [@JSX] Text.createElement(~text="Age", ())
  )>
  child
</Description>;

<Description
  term={
    <Text
      superLongPunnedProp
      anotherSuperLongOneCrazyLongThingHere
      text="Age"
    />
  }>
  child
</Description>;

<Foo
  bar={
    <Baz
      superLongPunnedProp
      anotherSuperLongOneCrazyLongThingHere
    />
  }
/>;

<div> <span> (str("hello")) </span> </div>;

<description term={<text text="Age" />}>
  child
</description>;

<description
  term=(text(~text="Age", ~children=[], ()))>
  child
</description>;
<description
  term=([@JSX] text(~text="Age", ~children=[]))>
  child
</description>;
<description
  term=([@JSX] text(~text="Age", ()))>
  child
</description>;

<description
  term={
    <div
      superLongPunnedProp
      anotherSuperLongOneCrazyLongThingHere
      text="Age"
    />
  }>
  child
</description>;

Module.[
  <Component> <div test="asd" /> </Component>,
];
Module.[<Component> <div /> </Component>];
Module.[<Foo> <Bar /> </Foo>];
Module.[<Component />];

let (/></) = (a, b) => a + b;

let x = foo /></ bar;

/* https://github.com/facebook/reason/issues/870 */
<div onClick=this##handleClick>
  <> foo </>
</div>;

<div onClick=this##handleClick>
  <> (foo(bar)) </>
</div>;

/* function application */
<div onClick=this##handleClick>
  <> (foo(bar)) </>
</div>;

/* tuple, not function application */
<div onClick=this##handleClick>
  <> foo bar </>
</div>;

/* https://github.com/facebook/reason/issues/2020 */
<div />;

<div> foo </div>;

<div />;

<Component
  accept=(
    fun
    | Foo => true
    | Bar => false
  )
/>;

<C
  prop=M.{
    a: "xxxxxxxxxxxxxxxxxxxxxx",
    b: "xxxxxxxxxxxxxxxxxxxxxx",
    c: "xxxxxxxxxxxxxxxxxxxxxx",
  }
/>;

<C
  prop=M.[
    "xxxxxxxxxxxxxxxxxxxxxx",
    "xxxxxxxxxxxxxxxxxxxxxx",
    "xxxxxxxxxxxxxxxxxxxxxx",
  ]
/>;

<C
  prop=M.(
    [|
      "xxxxxxxxxxxxxxxxxxxxxx",
      "xxxxxxxxxxxxxxxxxxxxxx",
      "xxxxxxxxxxxxxxxxxxxxxx",
    |]
  )
/>;

<C
  prop=M.(
    "xxxxxxxxxxxxxxxxxxxxxx",
    "xxxxxxxxxxxxxxxxxxxxxx",
    "xxxxxxxxxxxxxxxxxxxxxx",
  )
/>;

<C
  prop=M.(
    Foo(
      "xxxxxxxxxxxxxxxxxxxxxx",
      "xxxxxxxxxxxxxxxxxxxxxx",
      "xxxxxxxxxxxxxxxxxxxxxx",
    )
  )
/>;

/* https://github.com/facebook/reason/issues/2028 */
<Foo bar=M.([]) />;

<Foo bar=M.([])> M.([]) </Foo>;
