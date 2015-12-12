let file_contents = [
]
  @ [
    foo
  ] @ [
    bar
  ]

let _ =
  match s.src with
  | None   -> [
      zz
    ] + 2
  | Some s -> [ Variable (
      s_src,
      OpamFormat.make_string (OpamFilename.to_string s)
    );
      yy ];
    foo
  | Some s -> {
      fww =
        s_src,
        OpamFormat.make_string (OpamFilename.to_string s)
    ; gdd =
        yy
    }

let _ =
  [ x;
    y ]
  @ z

let _ =
  [
    x;
    y ]
  @ z

let _ = [
  x;
  y
]
  @ z
