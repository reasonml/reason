let foo = () => {
  switch (whatever) {
  | Some(_) =>
    [%expr
    fun
    | Some(_) => ()
    ]
  };
};

