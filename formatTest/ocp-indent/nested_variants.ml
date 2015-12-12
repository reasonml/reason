type tt =
  | A of
      int
  | B of
      string
  | C of
      float
  | D of
      char

type tt = [
  | `a of int
  | `blskdjf of
      float
  | `problem_cause of [ `more_brackets ]
  | `problematic_case of
      string
]
