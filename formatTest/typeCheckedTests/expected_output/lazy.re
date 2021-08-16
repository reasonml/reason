[@reason.version 3.7];
let myComputation =
  lazy({
    let tmp = 10;
    let tmp2 = 20;
    tmp + tmp2;
  });

type myRecord = {myRecordField: int};

let operateOnLazyValue = (lazy {myRecordField}) => {
  let tmp = myRecordField;
  tmp + tmp;
};

let result =
  operateOnLazyValue(
    lazy({myRecordField: 100}),
  );

type box('a) =
  | Box('a);

let lazy thisIsActuallyAPatternMatch = lazy(200);
let tmp: int = thisIsActuallyAPatternMatch;
let (lazy (Box(i)), x) = (
  lazy(Box(200)),
  100,
);
let tmp: int = i;

let myComputation = lazy(200);

let reallyLoooooooooooooongIdentifierThatSpansMoreThan50Cols = 200;

let foo =
  lazy(
    reallyLoooooooooooooongIdentifierThatSpansMoreThan50Cols
  );
