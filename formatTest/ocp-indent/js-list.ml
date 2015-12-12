(* mixed list styles *)
let cases =
  [ Group ("publishing", [
      basic_pre2
        ~name;
    ]); (* I think this line and the 2 preceding ones are indented one space too
           few by ocp-indent *)
    Group ("recovery", [
      basic_pre2
        ~name
    ]);
  ]
