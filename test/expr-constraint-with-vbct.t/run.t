Format expression type constraints when binding also has a type constraint
  $ refmt ./input.re | tee formatted.re
  /* Array literal with type constraint */
  let x: array(int) = ([|1, 2|]: array(int));
  
  /* List literal with type constraint */
  let x: list(int) = ([1, 2, 3]: list(int));
  
  /* Tuple with type constraint */
  let x: (int, string) = (
    (1, "a"): (int, string)
  );
  
  /* Function expression with type constraint */
  let f: int => int = (x => x + 1: int => int);
  
  /* Record with type constraint */
  type t = {a: int};
  let x: t = ({ a: 1 }: t);

Idempotency check
  $ refmt ./formatted.re | tee formatted_back.re
  /* Array literal with type constraint */
  let x: array(int) = ([|1, 2|]: array(int));
  
  /* List literal with type constraint */
  let x: list(int) = ([1, 2, 3]: list(int));
  
  /* Tuple with type constraint */
  let x: (int, string) = (
    (1, "a"): (int, string)
  );
  
  /* Function expression with type constraint */
  let f: int => int = (x => x + 1: int => int);
  
  /* Record with type constraint */
  type t = {a: int};
  let x: t = ({ a: 1 }: t);

  $ diff formatted.re formatted_back.re
