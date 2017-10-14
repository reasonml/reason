open Ocamlbuild_pack
open Ocamlbuild_plugin

let ext_obj = !Options.ext_obj;;
let x_o = "%"-.-ext_obj;;

let refmt = "refmt --print binary"
let add_printers_tag = "reason.add_printers"

let ocamldep_command' tags =
  let tags' = tags++"ocaml"++"ocamldep" in
  let specs =
    [ !Options.ocamldep;
      T tags';
      Ocaml_utils.ocaml_ppflags (tags++"pp:dep");
      A "-modules" ] in
  S specs

let impl_intf ~impl ?(intf_suffix=false) arg =
  let inft_suffix_specs =
    if intf_suffix
    then [ A "-intf-suffix"; P ".rei" ]
    else [] in
  inft_suffix_specs
  @
  [ A (if impl then "-impl" else "-intf");
    P arg ]

let compile_c ~impl ~native tags arg out =
  let tags =
    tags ++
    "ocaml" ++
    (if native then "native" else "byte") ++
    "compile" in
  let specs =
    [ if native then !Options.ocamlopt else !Options.ocamlc;
      A "-c";
      T tags;
      Ocaml_utils.ocaml_ppflags tags;
      Ocaml_utils.ocaml_include_flags arg;
      A "-pp"; P refmt;
      A "-o"; Px out ]
    @ impl_intf ~impl ~intf_suffix:true arg in
  Cmd (S specs)

let union_tags re cm tag =
  Tags.union (tags_of_pathname re) (tags_of_pathname cm)++"implem"+++tag

let byte_compile_re_implem ?tag re cmo env build =
  let re = env re and cmo = env cmo in
  Ocaml_compiler.prepare_compile build re;
  compile_c ~impl:true ~native:false (union_tags re cmo tag) re cmo

let native_compile_re_implem ?tag ?(cmx_ext="cmx") re env build =
  let re = env re in
  let cmi = Pathname.update_extensions "cmi" re in
  let cmx = Pathname.update_extensions cmx_ext re in
  Ocaml_compiler.prepare_link cmx cmi [cmx_ext; "cmi"] build;
  compile_c ~impl:true ~native:true (union_tags re cmx tag) re cmx

let compile_ocaml_interf rei cmi env build =
  let rei = env rei and cmi = env cmi in
  Ocaml_compiler.prepare_compile build rei;
  let tags = tags_of_pathname rei++"interf" in
  let native = Tags.mem "native" tags in
  compile_c ~impl:false ~native tags rei cmi

let ocamldep_command ~impl arg out env _build =
  let out = List.map env out in
  let out = List.map (fun n -> Px n) out in
  let out =
    match List.rev out with
    | ([] | [_]) as out -> out
    | last :: rev_prefix -> [Sh "|"; P "tee"] @ List.rev_append rev_prefix [Sh ">"; last] in
  let arg = env arg in
  let tags = tags_of_pathname arg in
  let specs =
    [ ocamldep_command' tags;
      A "-pp"; P refmt ]
    @ impl_intf ~impl arg
    @ out in
  Cmd (S specs)

;;

rule "rei -> cmi"
  ~prod:"%.cmi"
  ~deps:["%.rei"; "%.rei.depends"]
  (compile_ocaml_interf "%.rei" "%.cmi")
;;
rule "re dependecies"
  ~prods:["%.re.depends"; "%.ml.depends" (* .ml.depends is also needed since
    the function "prepare_link" requires .ml.depends *)]
  ~deps:(["%.re"])
  (ocamldep_command ~impl:true "%.re" ["%.re.depends"; "%.ml.depends"])
;;
rule "rei dependencies"
  ~prods:["%.rei.depends"; "%.mli.depends"]
  ~dep:"%.rei"
  (ocamldep_command ~impl:false "%.rei" ["%.rei.depends"; "%.mli.depends"])
;;
rule "re -> d.cmo & cmi"
  ~prods:["%.d.cmo"]
  ~deps:["%.re"; "%.re.depends"; "%.cmi"]
  (byte_compile_re_implem ~tag:"debug" "%.re" "%.d.cmo")
;;
rule "re & cmi -> cmo"
  ~prod:"%.cmo"
  ~deps:["%.rei"(* This one is inserted to force this rule to be skipped when
                   a .ml is provided without a .mli *); "%.re"; "%.re.depends"; "%.cmi"]
  (byte_compile_re_implem "%.re" "%.cmo")
;;
rule "re -> cmo & cmi"
  ~prods:["%.cmo"; "%.cmi"]
  ~deps:(["%.re"; "%.re.depends"])
  (byte_compile_re_implem "%.re" "%.cmo")
;;
rule "re & cmi -> d.cmo"
  ~prod:"%.d.cmo"
  ~deps:["%.rei"(* This one is inserted to force this rule to be skipped when
        a .re is provided without a .rei *); "%.re"; "%.re.depends"; "%.cmi"]
  (byte_compile_re_implem ~tag:"debug" "%.re" "%.d.cmo")
;;
rule "re & rei -> cmx & o"
  ~prods:["%.cmx"; x_o]
  ~deps:["%.re"; "%.ml.depends"; "%.cmi"]
  (native_compile_re_implem "%.re")
;;
