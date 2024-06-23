Test uchar escape lexing

  $ refmt ./input.re
  let x = "\u{1F42B}";
  let y = "\u{0}";
  let y = "\u{00}";
  let y = "\u{000}";
  let y = "\u{000000}";
  let y = "\u{0000E9}";
  let y = "\u{10FFFF}";
  
  let x = "\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}";
  let () = Format.eprintf("x: %s@.", x);
  
  // in a comment
  /* "\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}" */

check internal translation

  $ ocamlc -dparsetree -pp 'refmt --print binary' -intf-suffix .rei -impl input.re -o test
  [
    structure_item (input.re[3,2+0]..[3,2+20]) ghost
      Pstr_value Nonrec
      [
        <def>
          pattern (input.re[3,2+4]..[3,2+5])
            Ppat_var "x" (input.re[3,2+4]..[3,2+5])
          expression (input.re[3,2+9]..[3,2+20])
            Pexp_constant PConst_string("\240\159\144\171",(input.re[3,2+9]..[3,2+20]),None)
      ]
    structure_item (input.re[4,24+0]..[4,24+16]) ghost
      Pstr_value Nonrec
      [
        <def>
          pattern (input.re[4,24+4]..[4,24+5])
            Ppat_var "y" (input.re[4,24+4]..[4,24+5])
          expression (input.re[4,24+9]..[4,24+16])
            Pexp_constant PConst_string("\000",(input.re[4,24+9]..[4,24+16]),None)
      ]
    structure_item (input.re[5,42+0]..[5,42+17]) ghost
      Pstr_value Nonrec
      [
        <def>
          pattern (input.re[5,42+4]..[5,42+5])
            Ppat_var "y" (input.re[5,42+4]..[5,42+5])
          expression (input.re[5,42+9]..[5,42+17])
            Pexp_constant PConst_string("\000",(input.re[5,42+9]..[5,42+17]),None)
      ]
    structure_item (input.re[6,61+0]..[6,61+18]) ghost
      Pstr_value Nonrec
      [
        <def>
          pattern (input.re[6,61+4]..[6,61+5])
            Ppat_var "y" (input.re[6,61+4]..[6,61+5])
          expression (input.re[6,61+9]..[6,61+18])
            Pexp_constant PConst_string("\000",(input.re[6,61+9]..[6,61+18]),None)
      ]
    structure_item (input.re[7,81+0]..[7,81+20]) ghost
      Pstr_value Nonrec
      [
        <def>
          pattern (input.re[7,81+4]..[7,81+5])
            Ppat_var "y" (input.re[7,81+4]..[7,81+5])
          expression (input.re[7,81+8]..[7,81+20])
            Pexp_constant PConst_string("\000",(input.re[7,81+8]..[7,81+20]),None)
      ]
    structure_item (input.re[8,103+0]..[8,103+20]) ghost
      Pstr_value Nonrec
      [
        <def>
          pattern (input.re[8,103+4]..[8,103+5])
            Ppat_var "y" (input.re[8,103+4]..[8,103+5])
          expression (input.re[8,103+8]..[8,103+20])
            Pexp_constant PConst_string("\195\169",(input.re[8,103+8]..[8,103+20]),None)
      ]
    structure_item (input.re[9,125+0]..[9,125+20]) ghost
      Pstr_value Nonrec
      [
        <def>
          pattern (input.re[9,125+4]..[9,125+5])
            Ppat_var "y" (input.re[9,125+4]..[9,125+5])
          expression (input.re[9,125+8]..[9,125+20])
            Pexp_constant PConst_string("\244\143\191\191",(input.re[9,125+8]..[9,125+20]),None)
      ]
    structure_item (input.re[11,148+0]..[11,148+65]) ghost
      Pstr_value Nonrec
      [
        <def>
          pattern (input.re[11,148+4]..[11,148+5])
            Ppat_var "x" (input.re[11,148+4]..[11,148+5])
          expression (input.re[11,148+9]..[11,148+65])
            Pexp_constant PConst_string("\240\159\144\171\240\159\144\171\240\159\144\171\240\159\144\171\240\159\144\171\240\159\144\171",(input.re[11,148+9]..[11,148+65]),None)
      ]
    structure_item (input.re[12,215+0]..[12,215+38]) ghost
      Pstr_value Nonrec
      [
        <def>
          pattern (input.re[12,215+4]..[12,215+6])
            Ppat_construct "()" (input.re[12,215+4]..[12,215+6])
            None
          expression (input.re[12,215+9]..[12,215+38])
            Pexp_apply
            expression (input.re[12,215+9]..[12,215+23])
              Pexp_ident "Format.eprintf" (input.re[12,215+9]..[12,215+23])
            [
              <arg>
              Nolabel
                expression (input.re[12,215+25]..[12,215+34])
                  Pexp_constant PConst_string("x: %s@.",(input.re[12,215+25]..[12,215+34]),None)
              <arg>
              Nolabel
                expression (input.re[12,215+36]..[12,215+37])
                  Pexp_ident "x" (input.re[12,215+36]..[12,215+37])
            ]
      ]
  ]
  
  $ ./test
  x: 🐫🐫🐫🐫🐫🐫

