const refmt = require('../refmt')
console.log(refmt)

console.log(refmt.printRE(refmt.parseRE(`let f = (a) => a + 1; print_int(f(5))`)))
console.log(refmt.printREI(refmt.parseREI(`let f: (~a: string) => int`)))
console.log(refmt.printML(refmt.parseML(`let f a = a + 1 print_int @@ f 5`)))
console.log(refmt.printMLI(refmt.parseMLI(`val f : a:string -> int`)))

try {
  refmt.parseRE(`let f = 1111 if (a) {2}`)
} catch (e) {
  console.log(e)
}
try {
  refmt.parseMLI(`val f: `)
} catch (e) {
  console.log(e)
}

try {
  refmt.parseRE(`type X = Foo`)
} catch (e) {
  console.log(e)
}

console.log("=============== we're good! ===============")
