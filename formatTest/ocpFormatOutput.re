
let file_contents = [] @ [foo] @ [bar];

switch s.src {
  | None => [zz] + 2
  | Some s => {
      [
        Variable
          (TODO_REMOVE_AMBIGUITY__ s_src 
                                   (
                                     OpamFormat.make_string (
                                       OpamFilename.to_string s
                                     )
                                   ) __TODO_REMOVE_AMBIGUITY), 
        yy
      ];
      foo
    }
  | Some s => {
      fww: (
        s_src, 
        OpamFormat.make_string (
          OpamFilename.to_string s
        )
      ), 
      gdd: yy
    }
};

[x, y] @ z;

[x, y] @ z;

[x, y] @ z;
switch a {
  | b => cccccc [d [e]]
  | b' => ccccc' [d' [e']]
};
/* A */
type x = /* A */ | Foo /* B */ | Bar;

/* AA */
/* D */
let x = 3;

let module M = {
  /* M1 */
  let x = a;
  /* M2 */
  let y = b;
};

/* M3 */
/* M4 */
let f x =>
  if true {
    0
  } else if
    /* comment */
    false {
    1
  };

let g x =>
  if true {
    0
  } else if
    /* comment */
    false {
    1
  };

f x /* bla */ y /* bla */ z;

/* ending comments */exception IOError of int exn;

module type S =
  S with type map 'a 'b 'c := t 'a 'b 'c;

{
  let start_finaliser_thread () => ignore (
    Thread.create
      (
        fun () => Fn.forever (
          fun () =>
            switch (read_finaliser_queue ()) {
              | None => Thread.delay 1.0
              | Some f =>
                  Exn.handle_uncaught
                    exit::false f
            }
        )
      ) 
      ()
  );
  ()
};

/* Reason: Commenting out invalid parse */
/* module F */
/*     (A:A) */
/*     (B:B) */type t1 = {a: int, b: int => int, c: int};

let try_lock t =>
  wrap_mutex a.b (fun () => was_locked);

let blit_string_bigstring 
    src::src 
    src_pos::src_pos=? 
    src_len::src_len=? 
    dst::dst 
    dst_pos::dst_pos=? 
    () =>
  blit_common
    get_src_len::String.length 
    get_dst_len::length 
    blit::unsafe_blit_string_bigstring 
    src::src 
    src_pos::?src_pos 
    src_len::?src_len 
    dst::dst 
    dst_pos::?dst_pos 
    ();

let f = test bla Int32.to_string pack_signed_32;

let module S:
  S1 with type t = S1.t with
    type comparator = S.comparator = {};

/* Reason: Commenting out invalid parse */
/* let error_string message = error message () <:sexp_of< unit >> */
let unimplemented s => ();

let () =
  StdLabels.List.iter
    f::(
      fun (exc, handler) =>
        Conv.Exn_converter.add_auto
          finalise::false exc handler
    ) 
    ();

[
  Date.to_string date, 
  " ", 
  ...if is_utc {
       ["Z"]
     } else {
       bla
     }
];

/* val v */
/*   : t */
/*  */
{
  let module M = (
    val (m: (module S with type t =t'))
  );
  x
};

let (a, b, c) = d;

type t = t0 = {a: int};

type t2 = [ | `a | `b];

type t = private | A | B;

let module Make: S with type t = t' = {
  type contents = C.t;
};

let module Map_and_set_binable = {
  let module C: S with type t = t = X;
};

/* val v */
type compare =
  [ | `no_polymorphic_compare] => 
  [ | `no_polymorphic_compare];

{Parts.sign: sign, hr};

let module M (A: A) :{let bla: bla;} => {};

let marshal_blit:
  flags::list Marshal.extern_flags? => 
  'a => 
  pos::int? => 
  len::int? => 
  t => 
  int =
  ();

let daemonize 
    redirect_stdout::redirect_stdout=`Dev_null 
    redirect_stderr::redirect_stderr=`Dev_null 
    cd::cd="/" 
    umask::umask_value=default_umask 
    () => bla;

let add: t => (event => Time.t => unit) => a = ();

switch a {
  | A when b => c
  | A b when b => c
};

let module S:
  S1 with type t = S1.t with
    type comparator = S.comparator = {};

{
  let f x => bla
  and g x => bli;
  ()
};

include {
  exception Break = Break;
  let y = 2;
};

let should_check_can_sell_and_marking 
    regulatory_regime =>
  switch z {
    | `foo => {
        some_function argument;
        flu
      }
    | `foo => {
        some_function argument;
        flu
      }
  };

invalid_arg (
  sprintf
    "Dequeue.%s: index %i is not in [%d, %d]" 
    fname 
    i 
    (front_index buf) 
    (back_index buf)
);

let mem {ar, cmp} el => {
  let len = Array.length ar;
  len > 0 && {
    let rec loop pos => bla;
    blu
  }
};

let blit_to (type a) (blit: Blit.t Base.t a) => {
  ();
  fun t dst::dst dst_pos::dst_pos =>
    blit
      src::t.base 
      src_pos::t.pos 
      src_len::t.len 
      dst::dst 
      dst_pos::dst_pos 
      ()
};

type t 'a =
  Bin_prot.Type_class.writer 'a =
    {
      size: Size.sizer 'a, 
      write: Write_ml.writer 'a, 
      unsafe_write: Unsafe_write_c.writer 'a
    };

let create 
    message::
      message=(Pid.to_string (Unix.getpid ())) 
    close_on_exec::close_on_exec=true => xx;

let module Make_using_comparator 
           (Elt: Comparator.S) 
           :(
              S with type Elt.t = Elt.t with
                type Elt.comparator = Elt.comparator
            ) => {};

find_thread_count (
  In_channel.read_lines (
    "/proc/" ^
      string_of_int (Unix.getpid ()) ^ "/status"
  )
);

type variant = [
  | `Jan 
  | `Feb 
  | `Mar 
  | `Apr 
  | `May 
  | `Jun 
  | `Jul 
  | `Aug 
  | `Sep 
  | `Oct 
  | `Nov 
  | `Dec
];
/* this could be fixed, but we actually want to handle the first case
   differently for when there is only one case (see next examples) */
let f x =>
  fun | A => {
          x;
          2
        } 
      | B => {
          y;
          3
        };

/* if we were to fix to the case above, the second >>= would be below the _
   (test taken from js-fun) */
x >>= (
  fun x =>
    try x {
      | _ => () >>= ((fun x => x))
    }
);

/* (and also: the some_handling here would be below Not_found) */
try () {
  | Not_found => some_handling
};

let f x => x;

let f x => x;

let f g => g @@ (fun x => x);

let f g => g @@ (fun x => x);

/* the above should probably be consistent with: */
let f x y => y + (
  switch x {
    | A => 0
  }
);

let f x y => y + (
  switch x {
    | A => 0
  }
);

/* wich means we may over-indent even when the block is non-closable */
let f x y => y + (
  switch x {
    | A => 0
  }
);

let f x y => y + (
  switch x {
    | A => 0
  }
);

let f x y => y + (
  switch x {
    | A => 0
  }
);

{
  somefun (fun x => x);
  somefun (
    if bla {
      bli
    }
  );
  somefun (
    if bla {
      bli
    } else {
      blu
    }
  )
};

{
  a;
  b
};

/* Surprisingly, this is the indentation correpsonding to OCaml's interpretation
   of this code.  Indenting this accordingly may help users notice that they're
   doing something dubious. */
/* let b = `b */
/* let d = `d */
/* ;; */
/* let a = b */
/* function (_ : [ `c ]) -> d */
/* ;; */let f x =>
  fun | A
          when
            switch x {
              | A
              | B => true
              | _ => false
            } =>
          B 
      | A => x 
      | _ => B;

let f x =>
  if (
    switch x {
      | A => true
    }
  ) {
    1
  } else {
    0
  };

let f x =>
  switch x {
    | A => true
    | B => false
    | exception Not_found => false
    | C => true
    | exception (Failure _ | Invalid_argument _) =>
        true
    | exception (A | B)
    | exception B.Err
    | exception C.Types.Xxx "someparam" => false
  };

exception MyExn of string;
f "foo" g [1, 2];

let x =
  f 1 (x 3 || x f lor g lsl k lor g && g lsr g);

let f x => g (fun x => x) [] (x: x) y::?z () 0;

let f p::p g::g () k::k=? () => {
  let x = 0;
  p
};

let f = {
  for i in 0 to 1 {
    a;
    b
  };
  x
};

external : f 'a => x::int => t="b";

external : g x::t => s::i => d::t => unit="b2";

let f g::g=[] v x::x=0 l::l=? b => {
  let l = g b p::p l;
  c l::"foo" b p::p l::l;
  u v p::p l::l b f
};

let f () => g x y x::?y y::?w a::b;

let f () => f (
  fun () => {
    for i in 0 to 10 {
      g
    };
    x
  }
);

external : f int => int="foo";

let f () =>
  for i in 0 to 10 {
    g
  };

let f () => {x: 1, y: 2};

let f () => {x: 1, y: 2};

let f () => {x: 1, y: 2};

let f () => {x: 1, y: 2};

let f x =>
  if x {
    x
  } else {
    f @@ (
      fun () => {
        g;
        h
      }
    )
  };
/* Simple declaration : OK */
type t = ..;

type t +=
  | A
  | B;

/* But : */
type t = ..;

type t +=
  | A
  | B;

/* Inside modules :  same pb */
let module P = {
  type t = ..;
  type t +=
  | A
  | B;
};

let module Q = {type P.t +=
  | C
  | D;};

/* another one */
let module Q' = {type P.t +=
  | C = P.A
  | D;};

/* also */
let module M = {
  type t = ..;
  let a = 1;
  let b = 2;
};
type term _ =
  | Int of int :term int 
  | Add :term (int => int => int) 
  | App of (term ('b => 'a)) (term 'b) :term 'a;

let rec eval: type a. term a => a =
  fun | Int n => n 
      /* a = int */ 
      | Add => (fun x y => x + y) 
      /* a = int -> int -> int */ 
      | App
          __TODO_REMOVE_AMBIGUITY( f x )TODO_REMOVE_AMBIGUITY__ =>
          (eval f) (eval x);

/* eval called at types (b->a) and b for fresh b */
let two = eval (
  App
    (TODO_REMOVE_AMBIGUITY__ (
                               App
                                 (TODO_REMOVE_AMBIGUITY__ 
                               Add (Int 1) __TODO_REMOVE_AMBIGUITY)
                             ) 
                             (Int 1) __TODO_REMOVE_AMBIGUITY)
);

let rec sum: type a. term a => _ =
  fun x => {
    let y =
      switch x {
        | Int n => n
        | Add => 0
        | App
            __TODO_REMOVE_AMBIGUITY( f x )TODO_REMOVE_AMBIGUITY__ =>
            sum f + sum x
      };
    y + 1
  };

type typ _ =
  | Int :typ int 
  | String :typ string 
  | Pair of (typ 'a) (typ 'b) :typ ('a, 'b);

let rec to_string: type t. typ t => t => string =
  fun t x =>
    switch t {
      | Int => string_of_int x
      | String => Printf.sprintf "%S" x
      | Pair
          __TODO_REMOVE_AMBIGUITY( t1 t2 )TODO_REMOVE_AMBIGUITY__ => {
          let (x1, x2) = x;
          Printf.sprintf
            "(%s,%s)" 
            (to_string t1 x1) 
            (to_string t2 x2)
        }
    };

type eq _ _ = | Eq :eq 'a 'a;

let cast: type a b. eq a b => a => b =
  fun Eq x => x;

let rec eq_type:
  type a b. typ a => typ b => option (eq a b) =
  fun a b =>
    switch (a, b) {
      | (Int, Int) => Some Eq
      | (String, String) => Some Eq
      | (
          Pair
            __TODO_REMOVE_AMBIGUITY( a1 a2 )TODO_REMOVE_AMBIGUITY__, 
          Pair
            __TODO_REMOVE_AMBIGUITY( b1 b2 )TODO_REMOVE_AMBIGUITY__
        ) =>
          switch (eq_type a1 b1, eq_type a2 b2) {
            | (Some Eq, Some Eq) => Some Eq
            | _ => None
          }
      | _ => None
    };

type dyn = | Dyn of (typ 'a) 'a :dyn;

let get_dyn: type a. typ a => dyn => option a =
  fun a 
      (
        Dyn
          __TODO_REMOVE_AMBIGUITY( b x )TODO_REMOVE_AMBIGUITY__
      ) =>
    switch (eq_type a b) {
      | None => None
      | Some Eq => Some x
    };

{
  let f: type a. list a => int = fun _x => 42;
  f []
};

let nth t n =>
  if (n < 0) {
    None
  } else {
    let rec nth_aux:
      type b. t 'a b => int => option 'a =
      fun t n =>
        switch t {
          | Empty => None
          | Node
              __TODO_REMOVE_AMBIGUITY( a t )TODO_REMOVE_AMBIGUITY__ =>
              if (n = 0) {
                Some a
              } else {
                nth_aux t (n - 1)
              }
        };
    nth_aux t n
  };

let rec f: type a b. a =
  fun | _ => assert false
and g: type a. a =
  fun | _ => assert false;
if (cond1 && cond2) {
  ()
};

fun | _ when x = 2 && y = 3 =>
        if (a = b || b = c && c = d) {
          ()
        };
let module M:
  S with type a = b and type c = d and type e = f = {};
let all_equal = a = b && c = d && e = f;

/* this && should line up with previous one */
/* '=' seems to be relevant here */
x &&
  t.entity = entity && 
  t.clearing_firm = clearing_firm && 
  t.type_ = type_;
/* applicative_intf.ml */
let args = bar "A" @> baz "B" @> nil;

let args = bar "A" @> (
  baz_qux @@ (zap "D" @> nil)
);
let () = foo.bar <- f x y z;

let should_check_can_sell_and_marking 
    regulatory_regime =>
  switch z {
    | `foo => some_function argument
  };

/* The above typically occurs in a multi-pattern match clause, so the clause
   expression is on a line by itself.  This is the more typical way a long
   single-pattern match clause would be written: */
let should_check_can_sell_and_marking 
    regulatory_regime =>
  switch z {
    | `foo => some_function argument
  };

let f x => ghi x;

/* common */
let x =
  try x {
    | a => b
    | c => d
  };

let x =
  try x {
    | a => b
    | c => d
  };

let x =
  try x {
    | a => b
    | c => d
  };

let z = some_function argument;

let () = f a b c::c d;

let () = f a b c::1. d;

let () = My_module.f a b c::c d;

/* This last case is where Tuareg is inconsistent with the others. */
let () = My_module.f a b c::1. d;

let () =
  messages :=
    Message_store.create
      (Session_id.of_string "") 
      /* Tuareg indents these lines too far to the left. */ 
      "herd-retransmitter" 
      Message_store.Message_size.Byte;

let () = {
  raise (
    Bug (
      "foo" ^
        /* In this and similar cases, we want the subsequent lines to
                 align with the first expression. */
        "bar"
    )
  );
  raise (Bug ("foo" ^ "quux" ^ "bar"));
  raise (Bug (foo + quux ^ "bar"));
  raise (Bug (foo + quux ^ "bar"))
};

/* Except in specific cases, we want the argument indented relative to the
   function being called.  (Exceptions include "fun" arguments where the line
   ends with "->" and subsequent lines beginning with operators, like above.) */
let () =
  Some (
    Message_store.create
      s 
      "herd-retransmitter" 
      unlink::true 
      Message_store.Message_size.Byte
  );

/* We like the indentation of most arguments, but want to get back towards the
   left margin in a few special cases: */
foo (
  bar (
    fun x =>
      /* special: "fun _ ->" at EOL */
      baz
  )
);

/* assume no more arguments to "bar" */
foo a_long_field_name::(check (fun bar => baz));

foo a_long_field_name::(check (fun bar => baz));

foo (
  bar (
    quux (
      fnord (
        fun x =>
          /* any depth */
          baz
      )
    )
  )
);

/* We also wanted to tweak the operator indentation, making operators like <=
   not special cases in contexts like this:  */
assert (foo (bar + baz <= quux));

/* lined up under left argument to op,
                                           sim. to ^ above */
/* Sim. indentation of if conditions: */
if (a <= b) {
  ()
};

/* Comparisons are different than conditionals; we don't regard them as
     conceptually part of the [if] expression. */if (
                                        a <= b
                                        ) {
                                        ()
                                        };

/* We regard the outermost condition terms as conceptually part of the [if]
     expression and indent accordingly.  Whether [&&] or [||], conditionals
     effectively state lists of conditions for [then]. */if (
                                        Edge_adjustment.is_zero arb.cfg.extra_edge &&
                                        0. = sys.plugs.edge_backoff && 
                                        0. = zero_acvol_edge_backoff
                                        ) {
                                        0.
                                        } else {
                                        1.
                                        };

if (
  Edge_adjustment.is_zero arb.cfg.extra_edge &&
    0. = sys.plugs.edge_backoff && 
    0. = zero_acvol_edge_backoff
) {
  0.
} else {
  1.
};

{
  let entries =
    List.filter
      (Lazy.force transferstati) 
      f::(
        fun ts =>
          Pcre.pmatch pat::pat ts.RQ.description
      );
  x
};

/* combination of operator at BOL and -> at EOL: */
Shell.ssh_lines x |!
  List.map
    f::(
      f (
        g (
          fun x => {
            let (name, path) =
              String.lsplit2_exn on::'|' x;
            (
              String.strip name, 
              String.strip path
            )
          }
        )
      )
    );

/* open paren ending line like begin */
if (a (p ^\/ "s") [e] = Ok ()) {
  `S {
    let label count => sprintf "%d s" c ^ (
      if (c = 1) {
        ":"
      } else {
        "s"
      }
    );
    x
  }
};
let f =
  fun | zoo => {
          foo;
          bar
        };

let g =
  fun | zoo => {
          foo;
          bar
        };

let () =
  switch foo {
    | Bar => snoo
  };
let assigned_to u =>
  Deferred.List.filter
    (Request_util.requests ()) 
    f::(
      fun request =>
        if false {
          ()
        } else {
          status_request
            request::request 
            () 
            msg_client::no_msg
            >>= (
            fun status => not (
              up_to_date_user status u
            )
          )
        }
    );

let old_good = foo bar qaz *>>= (
  fun x => hey ho lala *>>= (
    fun y => return (x, y)
  )
);

let old_good = foo bar qaz +>>= (
  fun x => hey ho lala +>>= (
    fun y => return (x, y)
  )
);

/* generalizations based on Tuareg code */
let old_good = foo bar qaz *>>| (
  fun x => hey ho lala *>>> (
    fun y => foo bar qaz +>>| (
      fun x => hey ho lala +>>> (
        fun y => return (x, y)
      )
    )
  )
);
/* ocp-indent is not going to be confused by comment-embedded tokens. */
type t = {/* This is a comment */ a: int};

type t = {
  /* This is a comment : with a colon. */ 
  a: int
};

type t = {
  a: int, 
  /* with the :     second field */ 
  b: int
};

type t = {
  a: int, 
  b: int, 
  /* and : the third... */ 
  c: int
};

/* colon in CR comment messes Tuareg up */
type cfg = {
  foo: int, 
  /* ignore-CR someone: float? */ 
  bar: string
};

/* To be more precise about the Tuareg bug, it is the fact that the colon in the comment
is the first or second colon after the start of the record definition.  If the comment
occurs after the first 2 fields in the record everything is fine.

For example, this is OK: */
type t = {
  foo: int, 
  bar: string, 
  /* ignore-CR someone: float? */ 
  baz: string
};

/* but Tuareg messes this up */
type t = {
  foo: int, 
  /* ignore-CR someone: float? */ 
  bar: string
};

/* Now that we have support for {v v} and {[ ]}, reindent inside comments,
   unless they are explicitly delimited as code or pre-formatted text.  These
   three all end up flattened to the same level. */
/*
type t = {
  (* This is a comment *)
  a: int;
}
*/
/*
   type t = {
   (* This is a comment *)
   a: int;
   }
*/
/*
       type t = {
         (* This is a comment *)
         a: int;
       }
*/
/* Possible to-do warning: Star-prefixed lines are allowed and indented a little
   less, to line up with the star in the opening comment parenthesis.  Maybe we
   don't care enough about them to worry about it, though. */
/** Doc comment text should be aligned with the first line, so indented more
than otherwise. */
/* We're now using some ocamldoc block syntax to control indentation, and sweeks
and the rest of us have been relying on it, in and out of doc comments.

{[
let code =
should be reindented like code
so as to work also with vim
]}

    {v g
   This is totally verbatim text and shouldn't be reindented.  It
 probably doesn't matter what the indentation of the first line of a
verbatim block is.  But how will this be done in vim?
 xx
  yy
   zz
    c  v}

Does this even confront ocp-indent?  I think, when reindenting whole files,
source code blocks do confront ocp-indent.
*/
/* {v

(* comments embedded in verbatim sections *)
(* want to be able to verbatim-out big chunks of code *)

v} */
/* {v

non-comments in verbatim sections
duh

v} */
let module M = {
  let x = 0;
};

/* reference */
let module M = {
  let () = ();
};

/* If there's a blank line before this, at least, shouldn't it revert to the
       block-level indentation, even if it doesn't precede a declaration?  As
       long as the prior declaration is complete, I mean.  If there isn't a
       blank line, I can see associating the comment with the line before. */
let module M = {
  let () = ();
};

/* sim. */
let module M = {
  let () = ();
  /* no problem */
  let () = ();
};

/* val f : foo : int -> */
/*   -> bar_snoo : a b */
/*                 (* this comment is in the wrong place *) */
/*   -> unit */
/*  */
/* val f : foo : int -> */
/*   -> bar_snoo : a */
/*   (* this comment is in the right place [under discussion] *) */
/*   -> unit */
/*  */
/* (* The only difference is the type "a b" instead of "a" for the labeled value *)
(*    bar_snoo. *) */
let module M: {let v: t 'a => s => t 'a;} =
  /* ... */
  {};
type foo = int;

/* just in case */
/* These two shouldn't be indented differently, but are. */
type z = [ | `Bar of foo | `Foo];

/* a comment [expected to apply to `Foo as below] */
type z = [ | `Bar | `Foo];

/* a comment */
/* On second thought, I kind of like this way of thinking about this
indentation, even though it is kind of parasyntactic: */
type z =
  /* Applies to "[" or `Bar. */
  [ | `Bar of foo | `Foo];

/* Applies to "|" or `Foo.  Indented too much. */
type z =
  /* Applies to "[" or `Bar. */
  [ | `Bar | `Foo];

/* Applies to "|" or `Foo. */
/* The way we write code, that will line up more nicely. */
foo
  /* This is indented too far to the left */
  bar;

/* It looks to me like we generally want the comment to apply to the
     following line in most circumstances, including this one.  The default indent
for an empty line after a function application that isn't terminated with a
     ";" or something would probably also be in a bit, in anticipation of an
argument, although I don't think that's crucial. */
foo quux /* about bar */ bar /* about baz */ baz;
let rec check_header t =>
  if (Iobuf.length t.buf < header_length) {
    failwiths "Short packet" t !sexp_of_t
  }
and session t => {
  check_header t;
  Session_id.of_int_exn id_int
}
and length t => {
  let len = raw_length t;
  if (len = eos_marker) {
    0
  } else {
    len
  }
}
and sexp_of_t t =>
  /* something pretty for debugging */
  {
    let (lo, len) = (
      Iobuf.snapshot t.buf, 
      Iobuf.length t.buf
    );
    protect
      finally::(
        fun () => {
          Iobuf.Snapshot.rewind lo t.buf;
          Iobuf.resize t.buf len
        }
      ) 
      (fun () => ())
  };
/* preferred list style */
let z = f [y, foo f::(fun () => arg)];

let z = f [y, foo f::(fun () => arg)];

/* legacy list style */
[f (fun x => x), f (fun x => x), f (fun x => x)];

[f (fun x => x), f (fun x => x), f (fun x => x)];

[f (fun x => x), f (fun x => x), f (fun x => x)];

x >>= (
  fun x => (
             try x {
               | _ => ()
             }
           ) >>= (
    fun x =>
      try x {
        | _ => () >>= ((fun x => x))
      }
  )
);

let () = expr >>| (
  fun | x => 3 
      | y => 4
);

let () = expr >>| (
  fun z =>
    switch z {
      | x => 3
      | y => 4
    }
);

let () = expr >>| (
  fun z =>
    fun | x => 3 
        | y => 4
);

let () = my_func () >>= (
  fun | A => 0 
      | B => 0
);

let () = my_func () >>= (
  fun | A => 0 
      | B => 0
);

let () = expr >>| (
  fun | x => 3 
      | y => 4
);

let () = expr >>| (
  fun | x => 3 
      | y => 4
);

let f = {
  f >>= m (fun f x => y);
  z
};

let f = {
  f |> m (fun f x => y);
  z
};

let f = {
  f |> m (fun f x => y);
  z
};
let module M = Foo G H;

let module M =
  Foo
    G 
    {
      let x = ();
    } 
    H;

/* To me, this looks fine as it is.  The rule seems fine as "indent arguments by
   2".  To illustrate, with a case where the functor name is longer: */
let module M = Functor G H I;

include
  Foo
    {
      let x = ();
    } 
    {
      let y = ();
    };

include
  Foo
    {
      let x = ();
    } 
    {
      let y = ();
    };

include
  Foo
    {
      let x = ();
    } 
    {
      let y = ();
    };

include
  Persistent.Make
    {
      let version = 1;
    } 
    Stable.Cr_soons_or_pending.V1;

include
  Persistent.Make
    {
      let version = 1;
    } 
    Stable.Cr_soons_or_pending.V1;

include
  Persistent.Make
    {
      let version = 1;
    } 
    Stable.Cr_soons_or_pending.V1;

include
  Persistent.Make
    {
      let version = 1;
    } 
    Stable.Cr_soons_or_pending.V1;

let module M =
  Foo
    {
      let x = ();
    } 
    {
      let y = ();
    };

let module M: S = Make M;

let module M: S with type t := int = Make M;

let module Simple_command (Arg: {}) => {};

let module Simple_command (Arg: {}) => {};

let module Simple_command (Arg: {}) => {};

let module Simple_command (Arg: {}) => {};

let module Simple_command (Arg: {}) => {};
/* Get C.t and (r : S.t -> T.t) indented two chars right of their labels. */
type t =
  A.t => 
  bbb::C.t => 
  D.t => 
  e::(f::G.t => H.t) => 
  I.t => 
  jjj::[ | `K | `L] => 
  M.t => 
  nnn::[ | `O | `P] => 
  qqq::(r::S.t => T.t) => 
  U.t;
let foo 
    some 
    very 
    long 
    arguments 
    that 
    we 
    break 
    onto 
    the 
    next 
    line => {
  bar ();
  baz
};

/* The [some] above is indented less when [let foo] is the first line.  The
   problem goes away if there's anything on the line before [let foo]. */
/* The picture shows where we want the `=' to be.  However, Tuareg currently moves it over
   to line up with the arguments.

   Perhaps this is merely a personal preference, but that seems ugly to me.

   pszilagyi: It's consistent with other infix operators (although this is syntax) for it
   to be where you prefer. */
let foo arguments => bar;

let foo arguments => bar;

/* This program parses, but the [let] is indented incorrectly. */
let module M = {
  let module M: module type of M = {
    let x = ();
  };
};

/* Removing the [: module type of M] removes the bug. */
let parenthesized_let_tweak = {
  let sub value n l f =>
    case
      value::value 
      (
        message
          ("fix_sending_" ^ n) 
          length::(35 + 29 + l) 
          f
      );
  x
};

let parenthesized_let_tweak =
  f
    x::{
      let n = S.S.g s.S.s s::s;
      y
    };
/* mixed list styles */
let cases = [
  Group
    (TODO_REMOVE_AMBIGUITY__ "publishing" 
                             [
                               basic_pre2
                                 name::name
                             ] __TODO_REMOVE_AMBIGUITY), 
  /* I think this line and the 2 preceding ones are indented one space too
           few by ocp-indent */ 
  Group
    (TODO_REMOVE_AMBIGUITY__ "recovery" 
                             [
                               basic_pre2
                                 name::name
                             ] __TODO_REMOVE_AMBIGUITY)
];
/* Relatively low priority Jane Street indentation bugs. */
/* js-args */
/* uncommon */
let x =
  try x {
    | a => b
    | c => d
  };

let x =
  try x {
    | a => b
    | c => d
  };

/* js-comment */
let mk_cont_parser cont_parse => {
  ();
  fun _state str max_pos::max_pos pos::pos => {
    let len = (max_pos - pos) + 1;
    cont_parse pos::pos len::len str
  }
};

/* sexp parser is sensitive to
   absent newlines at the end of files. */
/* It would be nice if a partially completed ocamldoc code fragment inside a
   comment had the closing delimiter "]}" indented nicely before the comment is
   closed.  (This has to be the last comment in the file, to be partial.) */
/* Maybe add:
   {[
     val state : t -> [ `Unstarted | `Running | `Stopped ]
   ]}
*/let projection_files =
  Deferred.List.map x f::(fun p => ()) >>|
    String.split on::'\n';
let f: int => int = ();

type t = | A | B;

let height =
  fun | A => 0 
      | B => 1;

if x {
  y
} else if x {
  y
} else {
  z
};

type t = int => int;
let f =
  fun | _ => 0;

let f x =>
  switch x {
    | _ => 0
  };

let f =
  fun | _ => 0;

let f x =>
  switch x {
    | _ => 0
  };

let f x =>
  switch x {
    | _ => 0
  };

let check_price t =>
  fun | {
          Exec.trade_at_settlement: (
            None | Some false
          )
        } =>
          ();

let check_price t =>
  fun | simpler => () 
      | other => ();

/* Sometimes we like to write big alternations like this, in which case the
   comment should typically align with the following clause. */
let 0 =
  switch x {
    | A =>
        /* a */
        a
  };

let 0 =
  switch x {
    | A =>
        /* a */
        a
  };

a || (
  switch a {
    | a => true
    | b => false
  }
);
let f x => x >>| (
  fun x => g x >>| (fun x => h x)
);

let f x => x >>| (
  fun x => g x >>| (fun x => h x)
);

let f x => x |! (fun x => g x |! (fun x => h x));

let f x => x |! (fun x => g x |! (fun x => h x));

z (fun x => x) |! Validate.of_list;

/* Tuareg indents this line too far. */
/* Tuareg works correctly on this (if you drop the fun). */z x
                                        |! Validate.of_list;

/* jli found this great one.  Tuareg gets confused by the paren before List.map and
   indents |! way too far, under "k ^".  ocp-indent should shine, since it understands the
   syntax better. */
List.filter_opt [
  format
    old 
    (
      fun old => "removed: " ^ (
        List.map
          old 
          f::(
            fun (k, v) =>
              k ^ "=" ^ acl_to_string v
          ) |!
          String.concat sep::", "
      )
    )
];

/* (|>) = (|!) */
let f x => x |> (fun x => g x |> (fun x => h x));

let f x => x |> (fun x => g x |> (fun x => h x));

z (fun x => x) |> Validate.of_list;

/* Tuareg indents this line too far. */
/* Tuareg works correctly on this (if you drop the fun). */z x
                                        |> Validate.of_list;

/* jli found this great one.  Tuareg gets confused by the paren before List.map and
   indents |> way too far, under "k ^".  ocp-indent should shine, since it understands the
   syntax better. */
List.filter_opt [
  format
    old 
    (
      fun old => "removed: " ^ (
        List.map
          old 
          f::(
            fun (k, v) =>
              k ^ "=" ^ acl_to_string v
          ) |>
          String.concat sep::", "
      )
    )
];
let handle_query qs msg_client::_ => try_with (
  fun () =>
    if true {
      f >>| (fun () => `Done ())
    } else {
      false
    }
);

if false {
  ()
} else {
  assert_branch_has_node branch node >>| (
    fun () => {...t, node, floating}
  )
};
type x = {foo: int, bar: int};

let x = {...x, foo: 3, bar: 5};

let x = {/* blah blah blah */ foo: 3, bar: 5};

let x = [{...x, foo: 3, bar: 5}];

let x = [{/* blah blah blah */ foo: 3, bar: 5}];

let x = {...M.x, M.foo: 3};

let x = {...x, M.foo: 3};

let x = {M.foo: 3};

{...foo, Bar.field1: value1, field2: value2};

{...foo, Bar.field1: value1, field2: value2};

/* multicomponent record module pathname */
{A.B.a: b, c: d};
let () = f x [%sexp_of int] y;

/* y */
let z = some_function [%sexp_of foo];

let z = some_function argument;

let d = print_sexp [%sexp_of unit] ();
/* gigantic string with weird characters that causes trouble */
TEST_UNIT =
  eprintf
    "%s\n" 
    (
      remove_progress_bar "[============================================================  ] 04840 / 04841\r                                                                               \r[============================================================  ] 04841 / 04842\r[=======================================                       ] 05010 / 07826\r[========================                                      ] 05053 / 13052\r[=============================                                 ] 06807 / 14348\r[===============================                               ] 08203 / 16405\r[=================================                             ] 09418 / 17458\r[=================================                             ] 09566 / 17458\r[==================================                            ] 09631 / 17458\r[=================================                             ] 10200 / 18846\r[===========================                                   ] 10221 / 23043\r[=============================                                 ] 11016 / 23098\rmake[1]: Leaving directory `/mnt/local/sda1/mstanojevic/repos/live/submissions'"
    );

x;
/* s */
[%raise_structural_sexp
  "feature's tip is already an ancestor of new base" {
    feature_tip: (old_tip: Rev.t), 
    new_base: (new_base: Rev.t)
  }
];

[%raise_structural_sexp
  "feature's tip is already an ancestor of new base" {
    feature_tip: (old_tip: Rev.t), 
    new_base: (new_base: Rev.t)
  }
];
/* Indentation that Jane Street needs to think about and make precise.

   These are long term ideas, possibly even conflicting with other tests. */
/* js-args */
{
  let min_closing_backoff = 
    -. 
    (
      Hidden_float.expose (
        arb.cfg.base_edge @! Buy
      ) +.
        Hidden_float.expose (
          arb.cfg.base_edge @! Sell
        )
    )
  ;
  0
};

/* js-type */
/* The following tests incorporate several subtle and different indentation
   ideas.  Please consider this only a proposal for discussion, for now.

   First, notice the display treatment of "(,)" tuples, analogous to "[;]"
   lists.  While "(,)" is an intensional combination of "()" and ",", unlike
   "[;]" lists, we believe "(,)" isn't too big a departure.  Value expression
   analogies are included in js-type.ml, (meant to be) consistent with the
   proposed type indentation.

   Second, and more divergently, the proposed indentation of function types is
   based on the idea of aligning the arguments, even the first argument, even
   where that means automatically inserting spaces within lines.  This applies
   to the extra spaces in ":__unit" and "(____Config.Network.t" below.

   We believe this fits into a more general incorporation of alignment into
   ocp-indent, to replace our internal alignment tool with a syntax-aware one.
   We like to align things for readability, like big records, record types,
   lists used to build tables, etc.

   The proposal also includes indenting "->" in the circumstances below relative
   to the enclosing "()", by two spaces.  In a sense, this happens first, and
   then the first argument is aligned accordingly.  So, there's no manual
   indentation or spacing below. */
module type SIG = {
  let instances:
    unit => 
    Command.Spec.t
      (
        Config.Network.t => 
        list (App.t, Config.instance, Config.app) => 
        verbose::bool => 
        'm
      ) 
      'm; 
  let instances:
    unit => 
    Command.Spec.t
      (
        Config.Network.t => 
        list (App.t, Config.instance, Config.app) => 
        verbose::bool => 
        'm
      ) 
      'm; 
  /* presumed analog with stars */ 
  let instances: (
    unit, 
    Command.Spec.t
      (
        Config.Network.t, 
        list (App.t, Config.instance, Config.app), 
        bool, 
        'm
      ) 
      'm
  );
};
/* nested "try" */
try (
  try x {
    | e => e
  }
) {
  | e => e
};

/* indented too far */type t = S.s;

/* looks like a constructor to ocp-indent, which indents too far */
type t = s;

/* correct, because this doesn't look like a constructor to ocp-indent */
type t = | S;

/* correctly indented a little more, because... */
type t = | S;

/* we leave room for the vertical bar */
/* analogous value expressions, analogous to lists, some different from now */
[x, y];

[x, y];

(x, y);

(x, y);

(x, y);

[x, y];

(x, y);

[x, y];

(x, y);

[x, y];
let f x => stop >>>
  /* We don't do this as a matter of style, but the indentation reveals a common
     mistake. */
  (
    fun () => {
      don't_wait_for (close fd);
      bind fd
    }
  );

let f x => {
  stop >>>
    /* This is what was intended, which is indented correctly, although it's bad
     style on my part. */
    (fun () => don't_wait_for (close fd));
  bind
};
type t = | A | B of int | C;
let f x => x
and g x => x
and h x => x;

let rec f: 'a .'a => 'a = fun x => g x
and g: 'a .'a => 'a = fun x => h x
and h: 'a .'a => 'a = fun x => f x;
/* ... */{
           let open Option;
           indented_line
         };
let reset_cond =
  switch states {
    | [_] => (fun _ v _ => e_id v)
    | _ => (fun s v clk => false)
  };

/* … */let module M (S: S) => F.Make {
  let module G = {
    type t;
    include (Foo: Foo with type t := t);
    include (Bar: Bar with type t := t);
  };
};

let module M = {type t;};

let module Update: {
  let f: t 'a 'b => 'a => unit; 
  let g: t 'a 'b => 'a => unit; 
  let module M: C with type k = t; 
  let module G: C with type k := f; 
  type t;
} = {
  type t = int;
};

let module M:
  S with
    type t = x and type t' = y and type t' = y = {
  type t = int;
};

let module M:
  S with
    type t = x and type t' = y and type t' = y = {
  type t = int;
};

let module Make:
  (M: T) => 
  {let f: int => int; let g: int => int;} =
  functor () => {};

let module Store 
           (K: API.KEY) 
           (V: API.VALUE) 
           :(
              API.STORE with
                module K = K and module V = V
            ) => {};

let module K = K;
/* multiline-comments
     can be troublesome:
      let x =
        let y =
          f z
        in y
      indented code should be kept as is */();

/* what about multi-line
           comments that don't start a line ?
        */w;

let s1 = "a b c d\n         e f g h\n  i j k";

let s2 = "a b c d e f g h i j k l";

let s3 = "a b c d e f g h\n i j k l m";
type tt =
  | A of int 
  | B of string 
  | C of float 
  | D of char;

type tt = [
  | `a of int 
  | `blskdjf of float 
  | `problem_cause of [ | `more_brackets] 
  | `problematic_case of string
];
let module M = {
  let a = ();
  let a = ff (ff (ff (ff (ff (ff ())))));
  let a = [[[[[[]]]]]];
  let a = [ff [ff [ff [ff [ff [ff []]]]]]];
};
let x = {
  as _; 
  inherit class foo; 
  method bar = ()
};

class foo = {
  as _; 
  method x = 2; 
  inherit class bar;
};

class foo = {
  inherit class bar;
};
type predicate =
  | Pred_Byte | Pred_Native | Pred_Toploop;

{
  ...pkg, 
  package_version: projFile.version, 
  package_description: false, 
  package_requires: []
};
/** From http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual029.html#htoc172
    The first special comment of the file is the comment associated
    with the whole module.*/
/** Special comments can be placed between elements and are kept
    by the OCamldoc tool, but are not associated to any element.
    @-tags in these comments are ignored.*/
/*******************************************************************/
/** Comments like the one above, with more than two asterisks,
    are ignored. */
/** The comment for function f. */
let f: int => int => int;
/** The continuation of the comment for function f. */
/** Comment for exception My_exception, even with a simple comment
    between the special comment and the exception.*/
/* Hello, I'm a simple comment :-) */
exception My_exception of (int => int) int;
/** Comment for type weather  */
type weather =
  | Rain of int 
  /** The comment for construtor Rain */ 
  | Sun;
/** The comment for constructor Sun */
/** Comment for type weather2  */
type weather2 =
  | Rain of int 
  /** The comment for construtor Rain */ 
  | Sun;
/** The comment for constructor Sun */
/** I can continue the comment for type weather2 here
    because there is already a comment associated to the last constructor.*/
/** The comment for type my_record */
type my_record = {
  foo: int, 
  /** Comment for field foo */ 
  bar: string
};
/** Comment for field bar */
/** Continuation of comment for type my_record */
/** Comment for foo */
let foo: string;
/** This comment is associated to foo and not to bar. */
let bar: string;
/** This comment is assciated to bar. */
/** The comment for class my_class */
class my_class : {
  inherit cl; 
  val mutable tutu:
    /** A comment to describe inheritance from cl */
    /** The comment for attribute tutu */
    string; 
  val toto:
    /** The comment for attribute toto. */
    int; 
  val titi:
    /** This comment is not attached to titi since
        there is a blank line before titi, but is kept
        as a comment in the class. */
    string; 
  method toto:
    /** Comment for method toto */
    string; 
  method m:
    /** Comment for method m */
    float => int
};
/** The comment for the class type my_class_type */
class type my_class_type = {
  val mutable x:
    /** The comment for variable x. */
    int; 
  method m:
    /** The commend for method m. */
    int => int
};
/** The comment for module Foo */
let module Foo: {
  /** The comment for x */ 
  let x: int;
};
/** A special comment that is kept but not associated to any element */
/** The comment for module type my_module_type. */
module type my_module_type = {
  /** The comment for value x. */ 
  let x: int; 
  /** The comment for module M. */ 
  let module M: {
    /** The comment for value y. */ 
    let y: int;
  };
};
/* ... */
/** The comment for class my_class */
/* class my_class: */
/*   object */
/*     (** A comment to describe inheritance from cl *) */
/*     inherit cl */
/*  */
/*     (** The comment for the instance variable tutu *) */
/*     val mutable tutu = "tutu" */
/*     (** The comment for toto *) */
/*     val toto = 1 */
/*     val titi = "titi" */
/*     (** Comment for method toto *) */
/*     method toto = tutu ^ "!" */
/*     (** Comment for method m *) */
/*     method m (f : float) = 1 */
/*   end */
/** The comment for class type my_class_type */
class type my_class_type = {
  val mutable x:
    /** The comment for the instance variable x. */
    int; 
  method m:
    /** The commend for method m. */
    int => int
};
/** The comment for module Foo */
let module Foo: {
  /** The comment for x */ 
  let x: int;
};
/** A special comment in the class, but not associated to any element. */
/** The comment for module type my_module_type. */
module type my_module_type = {
  /* Comment for value x. */ 
  let x: int;
};
/* ... */
/** Starting bla doc */
type bla = | Hup /** The hup case */ | Hap;
/** The hap case */
/** Ending bla doc */
/** Starting bla doc */
type bla = | Hup /** The hup case */ | Hap;
/** The hap case */
/** Ending bla doc */
type hop;
/** Hop's documentation */
type mip = {fup: int, /** fup field */ fip: int};
/** fip field */
/** Mip's documentation */
type t = | Hey | Ho;
/** Let's go. */
type tp = [ | `Hey | `Ho];
/** Tp doc.
    Second line. */
/** Starting function f doc */
let f: 'a => 'b;
/** Ending function f doc. */
let g: 'a => t;
/** Function g doc.
    Second line. */
let g: 'a => [ | `Hey | `Ho];
/** Let's go
    Second line. */
let x: unit => unit;
/** Here are a couple examples of some of its many uses

    {v step (fun m v -> m ~foo:v)
       +> flag "-foo" no_arg : (foo:bool -> 'm, 'm) t
    v}
*/a
  /* {[ (* {v *) ]} {v v} */
  b;

/*
     {[
       while true do
         xx
       done
       (* this is totally crazy !!! *)
     ]}
  */();
let () =
  switch x {
    | `A => "A"
    | `B => "B"
  };
let f =
  switch x {
    | {x: 3} => {
        let x = 4;
        ()
      }
  };

let f =
  switch x {
    | X
    | Y
    | Z
    | U => 1
    | K => 2
  };

let f =
  switch x {
    | X when foo = bar => fff
    | Y when f = x && g = 3 => z
  };

let f () =>
  switch s {
    /* Parenthesized ident ? */
    | x => (x, d)
    /* Regular ident */
    | _ => g
  };

switch x {
  | X
  | Y => 1
  | X => {
      2;
      3
    }
  | A => 2
};

let f g =>
  /* haha */
  switch z {
    | Z
    | B _ => x
    | A
        __TODO_REMOVE_AMBIGUITY( a _ _ b )TODO_REMOVE_AMBIGUITY__ as x => {
        let x = f a
        and hr = f b;
        f
      }
  };

let unwind_to =
  switch t {
    | KType
    | KModule => true
    | Kblob => false
    | _ => true
  };

let f x =>
  switch x {
    | A
    | B
    | C => x
    | z =>
        switch z {
          | _ => (
              fun | x => x
            )
        }
  };

let fun_dep ulam =>
  fun | A
      | B
      | C => ();

let fun_dep ulam =>
  fun | A
      | B
      | C
      | D => ();

/* let _ = */
/*   (match bla */
/*    with bli) */let x = [%x f 3];

let x = [%x f 3 5];

let x = [%x f 3 5];

let x = [%xy f 3 5];

let x = [%x fg 3 5];

let x = [%x f 3 5];

let x = [%x f 3 5];

let x = 3 + [%f f];

let x = [%f f] * [%f f] + [%f f];

let x = [%f f 4 2] * [%f f 3 4];

let x = [%f f 2 3] * [%f f 3 4] + [%f f 2 3];

let x = [%f f 2 3] * [%f f 3 4] + [%f f 2 3];

let x = [%f f 2 3] + [%f f 3 4] * [%f f 2 3];

let x = [%f f 2 3] + [%f f 3 4] * [%f f 2 3];

let x = [%f f 2 3] + [%f f 3 4] + [%f f 2 3];

let x = [%f f 4 2] * [%f f 3 4];

let x = [%f.u f 4 2] * [%f.u f 3 4];
let x = 3;

[%%a
  let x = [3, 2]
];

let module S = {
  let x = 3;
  [%%b
    let x = [3, 2]
  ];
};

[%%c
  let x = [3, 2]; 
  [%%d
    let x = [3, 2]
  ];
];

[%%x 2 * 3 + x];

[%%x 2 + 3 * x];

[%%x 2];

[%%x.y 2];

[%%x.y 2];

[%%x.y 2];

[%%x 2];

let module S = {
  let x = 3;
  [%%x.y 2];
  [%%x.y 2];
  [%%x.y 2];
};
type t = {
  a: int, 
  /** blablabla */ 
  b: int, 
  /** blublublu */ 
  c: int
};

/** ccc */
[A, /* A */ B];

/* B */
type t = {x: t1, /* c1 */ /* c2 */ y: t2};
let read_raw_gen_ic read_pixel ic l c max => {
  let img = Index8.create c l;
  let greymap = {
    Color.max: max, 
    Color.map: {
      let make_grey i => {r: i, g: i, b: i};
      Array.init (max + 1) make_grey
    }
  };
  img.Index8.colormap <- greymap;
  for i in 0 to (l - 1) {
    for j in 0 to (c - 1) {
      Index8.set img j i (read_pixel ic)
    }
  };
  img
};

let func_darken_only org level => {
  let level = 255 - level;
  {
    r:
      if (org.r > level) {
        level
      } else {
        org.r
      }, 
    g:
      if (org.g > level) {
        level
      } else {
        org.g
      }, 
    b:
      if (org.b > level) {
        level
      } else {
        org.b
      }
  }
};

let f =
  fun | {f1: (Foo | Bar), f2: _, f3: (Foo | Bar)} => {
          f1: (Foo, Bar), 
          f2: xxx = yyy, 
          f3: (Foo, Bar)
        };

switch a {
  | {kind: x} => ()
  | {LibIndex.kind: x} => ()
};

let x = ({kind: x}, {LibIndex.kind: x});
let module M = {
  let () = ();
  let f x => 3;
  let () = ();
};

let () = ();
let () =
  if true {
    "bla"
  } else if
    true {
    "bli"
  } else {
    "blo"
  };

let () =
  if true {
    "bla"
  } else if
    true {
    "bli"
  } else {
    "hop"
  };

let () =
  if true {
    "hop"
  } else if
    true {
    "hap"
  } else {
    ();
    "bla"
  };

let () = {
  if x {
    y
  } else {
    (k, w)
  };
  z
};

let () =
  if x {
    a
  } else {
    let y = x / 42;
    y
  };

let () = {
  if x {
    a
  } else if y {
    b
  } else {
    blabla
  };
  x
};

let () =
  if x {
    a
  } else {
    switch y {
      | A => x
      | B => y
    }
  };

let () =
  if x {
    a
  } else {
    switch y {
      | A => x
      | B => y
    }
  };

let () =
  if x {
    a
  } else {
    fun x => y
  };
let () =
  if true {
    "bla"
  } else if
    true {
    "bli"
  } else {
    "blo"
  };

let () =
  if true {
    "bla"
  } else if
    true {
    "bli"
  } else {
    "hop"
  };

let () =
  if true {
    "hop"
  } else if
    true {
    "hap"
  } else {
    ();
    "bla"
  };

let () = {
  if x {
    y
  } else {
    (k, w)
  };
  z
};

let () =
  if x {
    a
  } else {
    let y = x / 42;
    y
  };

let () = {
  if x {
    a
  } else if y {
    b
  } else {
    blabla
  };
  x
};

let () =
  if x {
    a
  } else {
    switch y {
      | A => x
      | B => y
    }
  };

let () =
  if x {
    a
  } else {
    switch y {
      | A => x
      | B => y
    }
  };

let () =
  if x {
    a
  } else {
    fun x => y
  };
let () =
  if true {
    "bla"
  } else if
    true {
    "bli"
  } else {
    "blo"
  };

let () =
  if true {
    "bla"
  } else if
    true {
    "bli"
  } else {
    "hop"
  };

let () =
  if true {
    "hop"
  } else if
    true {
    "hap"
  } else {
    ();
    "bla"
  };

let () = {
  if x {
    y
  } else {
    (k, w)
  };
  z
};

let () =
  if x {
    a
  } else {
    let y = x / 42;
    y
  };

let () = {
  if x {
    a
  } else if y {
    b
  } else {
    blabla
  };
  x
};

let () =
  if x {
    a
  } else {
    switch y {
      | A => x
      | B => y
    }
  };

let () =
  if x {
    a
  } else {
    switch y {
      | A => x
      | B => y
    }
  };

let () =
  if x {
    a
  } else {
    fun x => y
  };
/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*/
/**
   Generic Ast Rewriter API.

   This module provides all usual traverse functions and some higher-level ones
   on any tree structure as long as we consider only one type of nodes

   @author Louis Gesbert
   @author Valentin Gatien-Baron
   @author Mathieu Barbin
*/
open TraverseInterface;
/* module type TRAVERSE_LIFT = */
/* sig */
/*   val foldmap : ('acc -> 'expr -> 'acc * 'expr) -> 'acc -> 'code_elt -> 'acc * 'code_elt  */
/* end */
/** Some Extensions */
let module Utils: {
  /** A generalisation of the type needed in S
      ('a, 'at, 'bt ,'b) sub
      'a may be expressions where identifiers are strings
      'b an expressions where identfiers are uniq
      In that case, ('a,'a,'b,'b) represents a function that deconstruct a string expression
      into a - list of string expression
             - a function that expects an ident expression list and build you the the 'original' ident expression

      DON'T LOOK at the types, it's too scary
      Instead take a look at the following example, where you build the subs_cons function for the expressions
      of some ast:
      let subs_cons e =
        match e with
        | Apply (e1,e2) ->
          (* (e1,e2) is a pair of expression and you are currently treating
           * expressions, you write exactly that: *)
          wrap (fun x -> Apply x) ((sub_2 sub_current sub_current) (e1,e2))
        | Match pel ->
          (* pel is a list of pattern * expr
           * we just ignore the pattern since there is no expression inside them
           * we stop the deconstruction on the expression, since it is was we are currently deconstructing *)
          wrap (fun x -> Match x) (sub_list (sub_2 sub_ignore sub_current) pel)
        | _ -> ...

  */ 
  type sub 'a 'at 'bt 'b =
    'a => (list 'bt => 'b, list 'at); 
  let sub_2:
    sub 'a1 'at 'bt 'b1 => 
    sub 'a2 'at 'bt 'b2 => 
    sub ('a1, 'a2) 'at 'bt ('b1, 'b2); 
  let sub_3:
    sub 'a1 'at 'bt 'b1 => 
    sub 'a2 'at 'bt 'b2 => 
    sub 'a3 'at 'bt 'b3 => 
    sub ('a1, 'a2, 'a3) 'at 'bt ('b1, 'b2, 'b3); 
  let sub_4:
    sub 'a1 'at 'bt 'b1 => 
    sub 'a2 'at 'bt 'b2 => 
    sub 'a3 'at 'bt 'b3 => 
    sub 'a4 'at 'bt 'b4 => 
    sub
      ('a1, 'a2, 'a3, 'a4) 
      'at 
      'bt 
      ('b1, 'b2, 'b3, 'b4); 
  let sub_list:
    sub 'a 'at 'bt 'b => 
    sub (list 'a) 'at 'bt (list 'b); 
  let sub_option:
    sub 'a 'at 'bt 'b => 
    sub (option 'a) 'at 'bt (option 'b); 
  let sub_current: sub 'a 'a 'b 'b; 
  let sub_ignore: sub 'a _ _ 'a; 
  let wrap:
    ('a => 'b) => 
    (list 'at => 'a, list 't) => 
    (list 'at => 'b, list 't);
};
/* HACK: tmp until we merge it into TRAVERSE_CORE for TraverseInterface,
   and rename it into TRAVERSE */
module type OLD_TRAVERSE = {
  type t 'pconstraint 'p = (_, _, _); 
  let traverse_iter:
    ((t 'p => unit) => t 'p => unit) => 
    t 'p => 
    unit; 
  let traverse_map:
    ((t 'p => t 'p) => t 'p => t 'p) => 
    t 'p => 
    t 'p; 
  let traverse_fold:
    (('a => t 'p => 'a) => 'a => t 'p => 'a) => 
    'a => 
    t 'p => 
    'a; 
  let traverse_foldmap:
    (
      ('a => t 'p => ('a, t 'p)) => 
      'a => 
      t 'p => 
      ('a, t 'p)
    ) => 
    'a => 
    t 'p => 
    ('a, t 'p); 
  let traverse_exists:
    ((t 'p => bool) => t 'p => bool) => 
    t 'p => 
    bool; 
  let traverse_forall:
    ((t 'p => bool) => t 'p => bool) => 
    t 'p => 
    bool; 
  let traverse_fold_context_down:
    (
      ('env => 'a => t 'p => 'a) => 
      'env => 
      'a => 
      t 'p => 
      'a
    ) => 
    'env => 
    'a => 
    t 'p => 
    'a; 
  let iter: (t 'p => unit) => t 'p => unit; 
  let iter_up: (t 'p => unit) => t 'p => unit; 
  let iter_down: (t 'p => unit) => t 'p => unit; 
  let map: (t 'p => t 'p) => t 'p => t 'p; 
  let map_up: (t 'p => t 'p) => t 'p => t 'p; 
  let map_down: (t 'p => t 'p) => t 'p => t 'p; 
  let fold:
    ('a => t 'p => 'a) => 'a => t 'p => 'a; 
  let fold_up:
    ('a => t 'p => 'a) => 'a => t 'p => 'a; 
  let fold_down:
    ('a => t 'p => 'a) => 'a => t 'p => 'a; 
  let foldmap:
    ('a => t 'p => ('a, t 'p)) => 
    'a => 
    t 'p => 
    ('a, t 'p); 
  let foldmap_up:
    ('a => t 'p => ('a, t 'p)) => 
    'a => 
    t 'p => 
    ('a, t 'p); 
  let foldmap_down:
    ('a => t 'p => ('a, t 'p)) => 
    'a => 
    t 'p => 
    ('a, t 'p); 
  let exists: (t 'p => bool) => t 'p => bool; 
  let exists_up: (t 'p => bool) => t 'p => bool; 
  let exists_down: (t 'p => bool) => t 'p => bool; 
  let find:
    (t 'p => bool) => t 'p => option (t 'p); 
  let find_up:
    (t 'p => bool) => t 'p => option (t 'p); 
  let find_down:
    (t 'p => bool) => t 'p => option (t 'p); 
  let findmap:
    (t 'p => option 'a) => t 'p => option 'a; 
  let findmap_up:
    (t 'p => option 'a) => t 'p => option 'a; 
  let findmap_down:
    (t 'p => option 'a) => t 'p => option 'a; 
  /** traverse all the nodes of the tree in an unspecified order */ 
  let traverse_fold_right:
    ((t 'b => 'a => 'a) => t 'b => 'a => 'a) => 
    t 'b => 
    'a => 
    'a; 
  /** [fold_up_combine ?combine f acc0 t] folds [f] from leaves with [acc0], combining
      accumulators from sub-trees with [combine] before calling [f].
      Default value for combine is (fun _ b -> b)
      <!> Be carefull be using this function without combine, lots of accs are lost */ 
  let fold_up_combine:
    combine::('a => 'a => 'a)? => 
    ('a => t 'b => 'a) => 
    'a => 
    t 'b => 
    'a; 
  /** Folds all the nodes of the tree in an unspecified order */ 
  let fold_right_down:
    (t 'b => 'a => 'a) => t 'b => 'a => 'a; 
  let foldmap_up_combine:
    combine::('a => 'a => 'a)? => 
    ('a => t 'b => ('a, t 'b)) => 
    'a => 
    t 'b => 
    ('a, t 'b); 
  /** Non-recursive versions, e.g. if you want to handle recursion yourself and have a default case */ 
  let map_nonrec: (t 'b => t 'b) => t 'b => t 'b; 
  let fold_nonrec:
    ('a => t 'b => 'a) => 'a => t 'b => 'a; 
  let foldmap_nonrec:
    ('a => t 'b => ('a, t 'b)) => 
    'a => 
    t 'b => 
    ('a, t 'b); 
  /** Just because we had fun writing it. Don't use as is, it's probably very slow.
      Applies the rewriting until fixpoint reached */ 
  let map_down_fix:
    (t 'b => t 'b) => t 'b => t 'b; 
  /** Additional functions that let you traverse the type 'c t when they are deep into an arbitrary structure 'b
      as long as you provide the functions to unbuild/rebuild 'b into t lists */ 
  type sub 'b 'c = Utils.sub 'b (t 'c) (t 'c) 'b; 
  let lift_iter_up:
    sub 'b 'c => (t 'c => unit) => 'b => unit; 
  let lift_iter_down:
    sub 'b 'c => (t 'c => unit) => 'b => unit; 
  let lift_map_up:
    sub 'b 'c => (t 'c => t 'c) => 'b => 'b; 
  let lift_map_down:
    sub 'b 'c => (t 'c => t 'c) => 'b => 'b; 
  /* like fold_map_up_for_real */ 
  let lift_fold_up_combine:
    sub 'b 'c => 
    combine::('a => 'a => 'a)? => 
    ('a => t 'c => 'a) => 
    'a => 
    'b => 
    'a; 
  let lift_fold:
    sub 'b 'c => 
    ('a => t 'c => 'a) => 
    'a => 
    'b => 
    'a; 
  let lift_fold_right_down:
    sub 'b 'c => 
    (t 'c => 'a => 'a) => 
    'b => 
    'a => 
    'a; 
  let lift_foldmap_up:
    sub 'b 'c => 
    ('a => t 'c => ('a, t 'c)) => 
    'a => 
    'b => 
    ('a, 'b); 
  let lift_foldmap_down:
    sub 'b 'c => 
    ('a => t 'c => ('a, t 'c)) => 
    'a => 
    'b => 
    ('a, 'b); 
  let lift_exists:
    sub 'b 'c => (t 'c => bool) => 'b => bool;
};
/** {6 First implementation} */
/** Functor giving you the usual traverse functions */
let module Make:
  (X: S) => OLD_TRAVERSE with type t 'a = X.t 'a;
/** Functor for map2, fold2, etc. */
let module MakePair:
  (Fst: S) => 
  (Snd: S) => 
  OLD_TRAVERSE with
    type t 'a = (Fst.t 'a, Snd.t 'a);
/** {6 Second implementation} */
/** For the second version (S2), you may do not want to write the optimised version of fold, map, iter
    in this case you can use this unoptimzed constructors, to get them from the foldmap_children function */
let module Unoptimized: {
  /** Simple recursion */ 
  type foldmap 'acc 't 't2 =
    ('acc => 't => ('acc, 't)) => 
    'acc => 
    't2 => 
    ('acc, 't2); 
  let iter:
    foldmap unit 't 't2 => 
    ('t => unit) => 
    't2 => 
    unit; 
  let map:
    foldmap unit 't 't2 => 
    ('t => 't) => 
    't2 => 
    't2; 
  let fold:
    foldmap 'acc 't 't2 => 
    ('acc => 't => 'acc) => 
    'acc => 
    't2 => 
    'acc; 
  /** Mutual recursion */ 
  type foldmapAB 'acc 'tA 'tB =
    ('acc => 'tA => ('acc, 'tA)) => 
    ('acc => 'tB => ('acc, 'tB)) => 
    'acc => 
    'tA => 
    ('acc, 'tA); 
  let iterAB:
    foldmapAB unit 'tA 'tB => 
    ('tA => unit) => 
    ('tB => unit) => 
    'tA => 
    unit; 
  let mapAB:
    foldmapAB unit 'tA 'tB => 
    ('tA => 'tA) => 
    ('tB => 'tB) => 
    'tA => 
    'tA; 
  let foldAB:
    foldmapAB 'acc 'tA 'tB => 
    ('acc => 'tA => 'acc) => 
    ('acc => 'tB => 'acc) => 
    'acc => 
    'tA => 
    'acc;
};
open TraverseInterface;
let module Make2:
  (X: S2) => 
  TRAVERSE with
    type t 'a = X.t 'a and 
    type container 'a = X.t 'a;
let module MakeLift1:
  (Y: LIFT2) => 
  (X:
     TRAVERSE with
       type container 'a = Y.t 'a and 
       type t 'a = Y.t 'a) => 
  TRAVERSE with
    type t 'a = X.t 'a and 
    type container 'a = Y.container 'a;
let module MakeLift2:
  (Y: LIFT2) => 
  (X: TRAVERSE with type container 'a = Y.t 'a) => 
  TRAVERSE with
    type t 'a = X.t 'a and 
    type container 'a = Y.container 'a;
/* From there, you can build Box of Boxes with MakeBox */
/* for example, for rewriting rules on a tuple of code, etc...*/
/** {6 Mutual Recursive Trees} */
let module MakeAB:
  (AB: AB) => 
  TRAVERSE_AB with
    type tA 'a = AB.tA 'a and 
    type tB 'a = AB.tB 'a;type a = | A
and b = int;

let module M = {
  type s = t
  and t = {foo: s};
};
type t 'a 'b = a::'a => b::b? => unit;

type t 'a 'b =
  | A | B of (t 'a 'b) 'k | C of 'a 'b;

type t = | Foo | Bar | Baz;

type t = | Foo | Bar | Baz;

type t = | Foo | Bar | Baz;

type t = | Foo | Bar | Baz;

type t = | Foo | Bar | Baz;

type t = {foo: int => int, bar: 'a};

type t = {x: int};

type t = {
  x: int, 
  y:
    int => 
    a::string => 
    b::(int => string)? => 
    unit, 
  mutable z: int, 
  mutable a: string => A.t unit
};

type t = {
  x: int, 
  y:
    int => 
    a::string => 
    b::(int => string)? => 
    unit, 
  mutable z: int, 
  a: string => A.t unit
};

type t = {
  x: int, 
  y:
    int => 
    a::string => 
    b::(int => string)? => 
    unit, 
  mutable z: int, 
  a: string => A.t unit
};

type t = {
  x: int, 
  y:
    int => 
    a::string => 
    b::(int => string)? => 
    unit, 
  mutable z: int, 
  mutable a: string => A.t unit
};

type t = {
  x: int, 
  y:
    int => 
    a::string => 
    b::(int => string)? => 
    unit, 
  mutable z: int, 
  mutable a: string => A.t unit
};

type t = [ | `a | `b | `c];

type t = [ | `a | `b | `c];

type t = [ | `a | `b | `c];

type t = [ | `a | `b | `c];

type t = [ | `a | `b | `c];

type t = [ | `a | `b | `c];

module type M = {
  type t = t0
  and t'
  and t'' = t; 
  let v: t;
};
/** ocaml language extensions
    (http://caml.inria.fr/pub/docs/manual-ocaml/manual021.html)
*/
/* other integer literals */
let i = 12l + 0l;

let i = 12L + 0l;

let i = 12n + 0n;

/* range patterns */
let f =
  fun | 'a'..'z' => e1 
      | 'A'..'Z'
      | '0'..'9' => e2;

/* local modules */
let f = {
  let module M = F {};
  M.f x
};

/* recursive modules */
let module rec M: S = {}
and M1: S1 = {};

/* private types */
type t = private | X of string | Y;

type t = private {f1: t1, f2: t2};

type t = private t';

/* local opens */
let module Res = F X;

{
  let open Res;
  ()
};

/* record shortcuts */
{
  let x = 1
  and y = 2;
  {x, y}
};

let f =
  fun | {x, y, _} => ();

/* locally abstract types */
let f (type t) (x: t) => ();

let f (type t) (x: t) => ();

/* first-class modules */
type m = ((module M.Sig with type t ='b), unit);

/* module type of */
module type S = {include module type of M;};

/* signature substitution */
module type S = {
  include M0 with type t := t; 
  let x: unit;
};

/* class overriding */
class cl = {
  as _; 
  inherit! class cl; 
  val! v = v; 
  method! m = m;
};

/* GADTs */
type t _ =
  | A :t int | B of (t 'a) (t 'b) :t ('a, 'b);
/** ocaml module expressions
    (http://caml.inria.fr/pub/docs/manual-ocaml/manual019.html)
*/
let module M = {};

let module M = {};

let module M (M1: T1) (M2: T2) => {};

let module M (M1: T1) (M2: T2) => {};

let module M (M1: T1) (M2: T2) => {};

let module M (M1: T1) (M2: T2) => {};

let module M = F X Y;

let module M: {} = {};

let module M: Sig = {};

let module M (X1: T1) (X2: T2) => {};
/** ocaml module types
    (http://caml.inria.fr/pub/docs/manual-ocaml/manual018.html)
*/
module type T = M.T;

module type T = {};

module type T = {};

module type T = (M: T) => (M1: T1) => {};

module type T =
  {} with
    type t 'a = 'b and 
    module M = M'.MF(X) and 
    type t' = t'';

module type T = {};

module type T = {
  let v: t; 
  external x: 'a="stub"; 
  type t = int
  and t2 = t; 
  exception Error of int; 
  class virtual cl 'a : {}
  and cl2 : {}; 
  class type clt = {}
  and clt2 'a = {}; 
  let module M: Sig; 
  let module M: (X: X) => (Y: Y) => Sig; 
  module type Sig; 
  module type Sig1 = {}; 
  open M; 
  include M;
};
/** ocaml patterns
    (http://caml.inria.fr/pub/docs/manual-ocaml/patterns.html)
*/
fun | x => () 
    | _ => () 
    | 'a' => () 
    | x as y => () 
    | (x: 'a => 'b) => () 
    | x
    | y => () 
    | Some x => () 
    | `Var x => () 
    | #ty => () 
    | (x, y) => () 
    | {f1: x, f2: y, f3: z, _} => () 
    | [x, y, z] => () 
    | [x, y, ...z] => () 
    | [|x, y, z|] => () 
    | (lazy w) => ();
/** ocaml type and exception definitions
    (http://caml.inria.fr/pub/docs/manual-ocaml/manual016.html)
*/
type t;

type t 'a;

type t +'_a;

type t -'a;

type t 'a +'b -'c -'d;

type t = t2;

type t = | A;

type t =
  | A 
  | B of 'a 
  | C of 'a 'b 
  | D of (Array.t 'a) (list 'b) 
  | E of _;

type t = {
  f1: t1, 
  f2: 'a, 
  mutable f3: t2, 
  f4: 'a 'b .t2
};

type t 'aconstraint 'a = tconstraint 'b = 'a;

type t 'a +'b -'c -'d = {
  f1: t1, 
  f2: 'a, 
  mutable f3: t2, 
  f4: (t1, t2)
}
constraint 'a = t
constraint 'b = 'a;

exception E;

exception E of (t 'a) string;

exception E' = E;
/** ocaml type expressions
    (http://caml.inria.fr/pub/docs/manual-ocaml/types.html)
*/
module type MySig = {
  /* variables */ 
  let a: 'ident; 
  let a: _; 
  /* parentheses */ 
  let a: t; 
  /* functions */ 
  let a: int => int => t => t => t; 
  let a: lab1::int => lab2::t => t; 
  let a: lab1::(l2::int? => t)? => t; 
  /* tuples */ 
  let a: ((t1, t2), t); 
  /* constructed */ 
  let a: int; 
  let a: Array.t ('a => 'b); 
  /* aliased */ 
  let a: int as 'bla; 
  /* polymorphic variants */ 
  let a: [ | `s | `t' | `_00 | `Aa of int]; 
  let a: [ | `s | `t' | `_00 | `Aa of int]; 
  let a: [< | `s | `t' | `_00 | `Aa of int]; 
  let a: [ | `s | `t' | `_00 | `Aa of int]; 
  let a: [<
    | `Bb of int &string &t 
    int 
    > `a `_bbb `c `d
  ]; 
  /* objects */ 
  let a: <>; 
  let a: <..>; 
  let a: <
    meth : option int, 
    meth2 : 'a .option 'a, 
    meth3 : 'a 'b .Hashtbl.t 'a 'b
  >; 
  let a: <
    meth : option int, 
    meth2 : 'a .option 'a, 
    meth3 : 'a 'b .Hashtbl.t 'a 'b, 
    ..
  >; 
  /* #-types */ 
  let a: #M.meth; 
  let a: #M.meth 'a; 
  let a: #M.meth 'a ('b, 'c);
};
type t = [ | `aaa | `bbb | `ccc];

type t = [ | `aaa | `bbb | `ccc];

type t = [ | `aaa | `bbb | `ccc];

type t = [ | `aaa | `bbb | `ccc];

type t = [ | `aaa | `bbb | `ccc];

type t = [ | `aaa | `bbb | `ccc];

type t = [ | `aaa | `bbb | `ccc];

type t = [ | `aaa | `bbb | `ccc];
let x =
  try y {
    | A => ()
    | B => ()
  };

let x =
  try y {
    | A => 0
    | B => 0
  };

let x =
  try y {
    | A => 0
    | B => 0
  };

let x =
  try y {
    | A => 0
    | B => 0
  };

{
  let x =
    try y {
      | A => 0
      | B => 0
    };
  let x =
    try y {
      | A => 0
      | B => 0
    };
  let x =
    try y {
      | A => 0
      | B => 0
    };
  let x =
    try y {
      | A => 0
      | B => 0
    };
  x
};
let f x =>
  switch x {
    | `A => "A"
    | `B => "B"
  };

let f =
  fun | `A => "A" 
      | `B => "B";

let f x =>
  switch x {
    | `A => "A"
    | `B => "B"
  };

let f = {
  let g x =>
    switch x {
      | `A => "A"
      | `B => "B"
    };
  g
};

let f = {
  let g =
    fun | `A => "A" 
        | `B => "B";
  g
};

let f = {
  let g x =>
    switch x {
      | `A => "A"
      | `B => "B"
    };
  g
};

let z =
  switch x {
    | X => x
  };

let config_converter = {
  (
    fun str =>
      try
        /* just check syntax */
        {
          ignore (
            IndentConfig.update_from_string
              IndentConfig.default str
          );
          `Ok str
        } {
        | Invalid_argument s => `Error s
      }, 
    ignore (
      IndentConfig.update_from_string
        IndentConfig.default str
    )
  );
  `Ok str
};

let f =
  try (
    switch a {
      | B => x
    }
  ) {
    | C => y
  };

let g =
  try (
    switch X {
      | X => X
    }
  ) {
    | X => Y
  };
