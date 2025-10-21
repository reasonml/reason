  $ refmt ./input.re
  let x1 = (()) => 1
  let x2 = (a) => 1
  let x3 = ((a: int)) => (b) => 1
  let x4 = ((a, b)) => 1
  let x5 = (a) => (b) => (1: int)
  let x6 = (~x) => (~y) => 1
  let x7 = (~x) => (~y) => 1
  let x8 = (~x=?5) => (~y=?) => (~z=?) => (()) => 1
  type a = int
  type b = float
  type c = string
  type t1 = a => b
  type t2 = a => b => c
  type t3 = (a, b) => c
  type t4 = ~x: int => ~y: string => c
  type t5 = ~x: a=? => b
  type tf = int => int => string
  type tNested2 = int => int => string
  type tNested3 = int => int => int => string
  type tNested4 = int => int => string
  type tNested5 = (int, int) => string
  type tNested7 = array(list(int) => string)
  type t6 = int
  type t7('a)  = list('a)
  type t8('a, 'b)  = (list('a), 'b)
  type t9 = t8(string, int)
  class type restricted_point_type = { pub get_x: int; pub bump: unit }
  class type t10('a)  = { pub thing: 'a }
  class type t11('a, 'b)  = { pub thing: ('a, list('b)) }
  module MyFirstModule = { let x = 0; type i = int and n = string }
  module type HasTT = { type tt }
  module SubModule = ({ type tt = int }: HasTT)
  module type HasEmbeddedHasTT = { module SubModuleThatHasTT: module SubModule }
  module type HasPolyType = { type t('a)  }
  module type HasDoublePoly = { type m('b, 'c)  }
  module type HasDestructivelySubstitutedPolyType = HasPolyType with type ('a) t := list('a)
  module type HasDestructivelySubstitutedSubPolyModule = { module X: HasDestructivelySubstitutedPolyType }
  module type HasSubPolyModule = { module X: HasPolyType }
  module EmbedsSubPolyModule = ({ module X = { type t('a)  = list('a) } }: HasSubPolyModule)
  module InliningSig = ({ let x = 10; let y = 20 }: { let x: int; let y: int })
  module MyFunctor = (M: HasTT) => { type reexportedTT = M.tt; let someValue = 1000 }
  module MyFunctorResult = MyFunctor({ type tt = string })
  module type ASig = { let a: int }
  module type BSig = { let b: int }
  module CurriedSugar = (A: ASig) => (B: BSig) => { let result = (+)(A.a, B.b) }
  type withThreeFields = { name: string, age: int, occupation: string }
  let testRecord = { name: "joe", age: 20, occupation: "engineer" }
  let makeRecordBase = (()) => { name: "Joe", age: 30, occupation: "Engineer" }
  type t = | A | B(int) | C(int, int) | D((int, int))
  type foo = { x: int }
  let result = Some({ x: 1 })
  type tt1 = | A(int) | B(bool, string)
  type tt2 = | A(int) | B((bool, string))
  type tt3 = [ | `A(int) | `B((bool, string)) | `C ]
  type tt4 = [ | `A(int) | `B((bool, string)) | `C ]
  let (=) = 0
  let (==) = 0
  let (<>) = 0
  let (!=) = 0
  type foobar(_)  = | Foo('a): foobar(unit)
  type expr(_)  = | Int(int): expr(int) | String(string): expr(string) | Pair('a, 'b): expr(('a, 'b))
  type point = | Point({ x: int, y: int })
  type person = | Person({ name: string, age: int }) | Anonymous
  type covariant(+'a)  = list('a)
  type contravariant(-'a)  = 'a => unit
  type mixed(+'a, -'b)  = 'a => 'b => unit
  type open_variant = [ > | `A | `B(int) ]
  type open_with_values = [ > | `Red | `Blue(string) | `Green((int, int)) ]
  exception GenericError('a)
  let with_extension = [%test ...]
  let with_complex = [%derive.show ...]
  [%%toplevel_ext ...]
  [%%foo ...]
  let rec foo = (a) => (b) => (+)(a, b) and bar = (a) => (b) => foo((-)(a, b))
