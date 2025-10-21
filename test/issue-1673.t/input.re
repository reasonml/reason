module type Screen = (Config: {type transition;}) => {
  type state;
  let run: (state, env) => Config.transition;
}

