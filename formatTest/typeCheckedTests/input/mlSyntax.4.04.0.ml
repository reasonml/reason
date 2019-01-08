module Foo = struct
  type t = { name: string }
end

let foo Foo.{name} = ()
