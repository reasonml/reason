Format issue #2843 - Record formatting improvements
  $ refmt ./input.re | tee formatted.re
  let applyHighlight =
      (
        spec: list(ChartSpecT.specItem(string)),
        ~highlightedKey: option(string),
      ) => {
    let defaultItemOpacity = item =>
      ChartSpecT.{
        ...item,
        chart: {
          ...item.chart,
          strokeOpacity: None,
        },
      };
  
    spec->List.map(defaultItemOpacity);
  };
  
  Param.create(
    ~key="search",
    ~toString=Params.Search.toString,
    ~fromString=Params.Search.fromString,
    ~get=state => state.search,
    ~set=
      (value, state) =>
        {
          ...state,
          filter: {
            ...state.filter,
            search: value,
          },
        },
  );

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

