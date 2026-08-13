module F = struct
  type t = int
  type s = string
  type r = unit
end

type ('a, 'b, 'c) triple = 'a * 'b * 'c

type paren = F.(t * s * r)
type single = F.(t)
type variant = F.[ `X of t | `Y of s | `Z of r ]
type nested = F.(F.(t) * s)
