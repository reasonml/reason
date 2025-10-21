m
|> StringMap.add("a", 1)
|> StringMap.fold_right(
  (el, m) => StringMap.add(el, 1, m),
  ["someReallyLongString"]
)
|> StringMap.add("h", 1);

