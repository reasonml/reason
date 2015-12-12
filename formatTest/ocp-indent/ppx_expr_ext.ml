let x =
  [%x f 3 ]

let x =
  [%x (f
         3
         5) ]

let x =
  [%x f
        3
        5 ]

let x =
  [%xy f
         3
         5 ]

let x =
  [%x fg
        3
        5 ]

let x =
  [%x   f
          3
          5 ]

let x =
  [%x
    f
      3
      5
  ]

let x =
  3 +
  [%f f ]

let x =
  [%f f ] * [%f f ]
  +
  [%f f ]

let x =
  [%f f
        4
        2 ]
  *
  [%f f
        3
        4 ]

let x =
  [%f f
        2
        3 ] * [%f f
                    3
                    4 ] +
  [%f f
        2
        3 ]

let x =
  [%f f
        2
        3 ] * [%f f
                    3
                    4 ]
  + [%f f
          2
          3 ]

let x =
  [%f f
        2
        3 ] + [%f f
                    3
                    4 ] *
              [%f f
                    2
                    3 ]

let x =
  [%f f
        2
        3 ] + [%f f
                    3
                    4 ]
              * [%f f
                      2
                      3 ]

let x =
  [%f f
        2
        3 ] + [%f f
                    3
                    4 ]
  + [%f f
          2
          3 ]


let x =
  [%
      f f
          4
          2 ]
  *
  [%
      f
    f
      3
      4 ]

let x =
  [%
      f
      .u f
           4
           2 ]
  *
  [%
      f
      .u
    f
      3
      4 ]

