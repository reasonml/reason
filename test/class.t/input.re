class aClass1(x) {
  /* one value parameter x */
  pub a1 = 0;
  pub a2() = 0;
  pub a3(x,y) = x + y;
  pub a4(x,y) {
    let result = x + y;
    print_endline(" x + y = " ++ string_of_int(x) ++ " + " ++ string_of_int(y) ++ " = " ++ string_of_int(result));
    result
  };
};
class aClass2(x) {
};
class aClass3(x: (int => int)) {
};
class aClass4(x: (int => int => int)) {
};
class aClass5(x: (int => (int => int))) {
};
class aClass6(x: ((int => int) => int)) {
};
class aClass7(x: ((int, int) => int)) {
};

class labeledClass1(~x) {
};
class labeledClass2(~x: ((~y:int) => int)) {
};
class labeledClass3(~x: ((~y:int) => int => int)) {
};
class labeledClass4(~x: ((~y:int) => (int => int))) {
};
class labeledClass5(~x: (((~y:int) => int) => int)) {
};
class labeledClass6(~x: ((~y:(int, int)) => int)) {
};
