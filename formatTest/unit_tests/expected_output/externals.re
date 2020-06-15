/**
 * Tests external formatting.
 */
external foo: type_ = "%caml_something_or_other";

external multilineStringExtern: int => int =
  "\n Did you know you can put whatver you want inside\n of an extern? Good luck with the linker though!\n";
