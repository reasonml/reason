let x = 3

[%% a
  let x = [
    3;
    2;
  ]
]

module S = struct

  let x = 3

  [%% b
    let x = [
      3;
      2;
    ]
  ]

end

[%% c
  let x = [
    3;
    2;
  ]

  [%% d
    let x = [
      3;
      2;
    ]
  ]

]

[%% x
  2 * 3
  +
  x
]

[%% x
  2 + 3
      *
      x
]

[%% x
  2
]

[%% x
    .
    y
  2
]


[%%   x
      .y
  2
]

[%% x .
    y
  2
]

[%%
    x
  2
]

module S = struct

  let x = 3

  [%% x
      .y
    2
  ]

  [%%   x
        .y
    2
  ]

  [%%
      x
      .y
    2
  ]

end
