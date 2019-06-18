

type result('a, 'b) = Ok('a) | Error('b);

let returnsAResultToBeUnwrapped = (a, b, c) => switch(a, b, c) {
  | (None, Some(i), Some(j)) => Ok((i, j));
  | (Some(i), Some(j), None) => Ok(("hi", "bye"));
  | _ => Error(Invalid_argument("This is not a valid argument"))
};

let returnsAnIntegerResultNotWrapped = (a, b, c) => switch(a,b, c) {
  | (None, Some(i), Some(j)) => 0
  | (Some(i), Some(j), None) => -1
  | _ => 100
};

let returnsABoolReturnValueNoResult = (a, b, c) => switch(a,b, c) {
  | (None, Some(i), Some(j)) => true
  | (Some(i), Some(j), None) => false
  | _ => false
};

let (!!) = res => switch(res) {
  | Ok(o) => o
  | Error(e) => raise(e)
};


let result = !!returnsAResultToBeUnwrapped(
  Some("this realy long string will make things wrap"),
  Some("this realy long string will make things wrap"),
  Some("this realy long string will make things wrap")
);

let result = -returnsAnIntegerResultNotWrapped(
  Some("this realy long string will make things wrap"),
  Some("this realy long string will make things wrap"),
  Some("this realy long string will make things wrap")
)

let result = +returnsAnIntegerResultNotWrapped(
  Some("this realy long string will make things wrap"),
  Some("this realy long string will make things wrap"),
  Some("this realy long string will make things wrap")
)

let result = !returnsABoolReturnValueNoResult(
  Some("this realy long string will make things wrap"),
  Some("this realy long string will make things wrap"),
  Some("this realy long string will make things wrap")
);
