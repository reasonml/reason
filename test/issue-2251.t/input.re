[@genType]
let renameABunch = ~pad =>
  [@genType.as "xRenamed"] (~x) => [@genType.as "yRenamed"] (~y) => pad+x+y;

