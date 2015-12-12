let _ =
  (* multiline-comments
     can be troublesome:
      let x =
        let y =
          f z
        in y
      indented code should be kept as is *)
  ()

let _ = (* what about multi-line
           comments that don't start a line ?
        *)
  w

let s1 = "a b c d
         e f g h
  i j k"

let s2 = "a b c d \
          e f g h \
          i j k\
         \ l"

let s3 = "a b c d \
          e f g h
 i j k \
          l m"
