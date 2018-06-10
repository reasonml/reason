(** rewrites `a -> f(b)` as `[@reason.fast_pipe] f(a, b)` *)
val parseFastPipe : Ast_404.Parsetree.expression -> Ast_404.Parsetree.expression -> Ast_404.Parsetree.expression

(** rewrites `[@reason.fast_pipe] f(a, b)` as `a -> f(b)` *)
val unparseFastPipe : Ast_404.Parsetree.expression -> Ast_404.Parsetree.expression
