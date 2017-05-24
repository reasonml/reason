module TestModule = {
  type twostrings = (string, string);
  let mkPair(s) {
    (s, s);
  }
};

let twoStrings = TestModule.mkPair("hello");
let () =
print_endline(TestModule.show_twostrings(twoStrings));
