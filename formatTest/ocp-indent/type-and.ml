type a =
  | A
and b = int

module M = struct
  type s = t and t = {
    foo : s;
  }
end
