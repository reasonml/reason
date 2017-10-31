(*  
  This file's shared between the Reason repo and the BuckleScript repo. In
  Reason, it's in src. In BuckleScript, it's in vendor/reason We periodically
  copy this file from Reason (the source of truth) to BuckleScript, then
  uncomment the #if #else #end cppo macros you see in the file. That's because
  BuckleScript's on OCaml 4.02 while Reason's on 4.04; so the #if macros
  surround the pieces of code that are different between the two compilers.
 *)
(* #if undefined BS_NO_COMPILER_PATCH then *)
(* val ast_mapper : Ast_mapper.mapper *)
(* #end *)
