type t1 =
  {
    a: int;
    b: int -> int;
    c: int;
  }

let try_lock t =
  wrap_mutex a.b (fun () ->
      was_locked)

let blit_string_bigstring ~src ?src_pos ?src_len ~dst ?dst_pos () =
  blit_common
    ~get_src_len:String.length ~get_dst_len:length
    ~blit:unsafe_blit_string_bigstring
    ~src ?src_pos ?src_len ~dst ?dst_pos
    ()

let f =
  test bla Int32.to_string
    pack_signed_32

module S : S1
  with type t = S1.t
  with type comparator = S.comparator = struct end

(* Reason: Commenting out invalid parse *)
(* let error_string message = error message () <:sexp_of< unit >> *)
let unimplemented s = ()

let () =
  StdLabels.List.iter
    ~f:(fun (exc, handler) ->
        Conv.Exn_converter.add_auto ~finalise:false exc handler)
    ()

let _ =
  Date.to_string date
  :: " "
  :: (if is_utc then ["Z"]
      else bla)

(* val v *)
(*   : t *)
(*  *)
let _ =
  let module M = (val m : S with type t = t') in
  x

let a,b,c =
  d

type t = t0 = {
  a: int;
}

type t2 = [
  | `a
  | `b
]

type t = private
  | A
  | B

module Make : (S with type t = t') =
struct
  type contents = C.t
end

module Map_and_set_binable = struct
  module C : (S with type t = t) = X
  (* val v *)
end

type compare =
  [`no_polymorphic_compare]
  -> [`no_polymorphic_compare]

let _ =
  {Parts.
    sign = sign;
    hr   = hr;
  }

module M (A:A) : sig
  val bla : bla
end = struct
end

let marshal_blit :
  ?flags : Marshal.extern_flags list -> 'a ->
  ?pos : int -> ?len : int -> t -> int = ()

let daemonize ?(redirect_stdout=`Dev_null) ?(redirect_stderr=`Dev_null)
    ?(cd = "/") ?umask:(umask_value = default_umask) () =
  bla

let add :
  t ->
  (event -> Time.t -> unit) ->
  a = ()

let _ = match a with
  | A
    when b -> c
  | A b
    when b -> c

module S : S1
  with type t = S1.t
  with type comparator = S.comparator = struct end

let _ =
  let f x =
    bla
  and g x =
    bli
  in ()

include struct
  exception Break = Break
  let y = 2
end

let should_check_can_sell_and_marking regulatory_regime =
  match z with
  | `foo
    -> some_function
         argument;
    flu
  | `foo -> some_function
              argument;
    flu

let _ =
  invalid_arg
    (sprintf "Dequeue.%s: index %i is not in [%d, %d]"
       fname i (front_index buf) (back_index buf))

let mem { ar; cmp } el =
  let len = Array.length ar in
  len > 0 &&
  let rec loop pos =
    bla
  in
  blu

let blit_to (type a) (blit : (Base.t, a) Blit.t) =
  (); fun t ~dst ~dst_pos ->
    blit ~src:t.base ~src_pos:t.pos ~src_len:t.len ~dst ~dst_pos ()

type 'a t = 'a Bin_prot.Type_class.writer
= { size : 'a Size.sizer;
    write : 'a Write_ml.writer;
    unsafe_write : 'a Unsafe_write_c.writer;
  }

let create
    ?(message = Pid.to_string (Unix.getpid ()))
    ?(close_on_exec=true)
  =
  xx

module Make_using_comparator (Elt : Comparator.S)
  : S with type Elt.t = Elt.t
    with type Elt.comparator = Elt.comparator = struct end

let _ =
  find_thread_count
    (In_channel.read_lines
       ("/proc/" ^ string_of_int (Unix.getpid ()) ^ "/status"))

type variant = [ `Jan | `Feb | `Mar | `Apr | `May | `Jun
               | `Jul | `Aug | `Sep | `Oct | `Nov | `Dec ]
