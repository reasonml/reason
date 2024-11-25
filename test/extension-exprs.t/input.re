/* Extension with pexp_apply */

[%defer
  cleanup();
];

/* Extension with comment with pexp_apply */

[%defer
  /* 2. comment attached to expr in extension */
  cleanup();
];

/* Let sequence + extension with pexp_apply */

let () = {
  /* random let binding */
  let x = 1;
  /* 1. comment attached to extension */
  [%defer cleanup()];
  /* 3. comment attached to next expr */
  something_else();
};

/* Let sequence + extension with comment with pexp_apply */

let () = {
  /* random let binding */
  let x = 1;
  /* 1. comment attached to extension */
  [%defer
   /* 2. comment attached to expr in extension */
   cleanup()
  ];
  /* 3. comment attached to next expr */
  something_else();
};
