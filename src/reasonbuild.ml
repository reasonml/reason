open Ocamlbuild_pack
open Ocamlbuild_plugin

let ext_obj = !Options.ext_obj;;
let x_o = "%"-.-ext_obj;;

let reasonfmt = "reasonfmt"
let ocamldep_command' tags =
let tags' = tags++"ocaml"++"ocamldep" in
  S [!Options.ocamldep; T tags'; Ocaml_utils.ocaml_ppflags (tags++"pp:dep"); A "-modules"]

let ocamlc_c tags arg out impl =
let tags = tags++"ocaml"++"byte" in
Cmd (S [!Options.ocamlc; A"-c"; T(tags++"compile");
       Ocaml_utils.ocaml_ppflags tags; Ocaml_utils.ocaml_include_flags arg;
       A "-pp"; P reasonfmt; A"-o"; Px out; if impl then A"-impl" else A"-intf-suffix"; P arg])

let byte_compile_re_implem ?tag re cmo env build =
  let re = env re and cmo = env cmo in
  Ocaml_compiler.prepare_compile build re;
  ocamlc_c (Tags.union (tags_of_pathname re) (tags_of_pathname cmo)++"implem"+++tag) re cmo true

let ocamlopt_c tags arg out impl =
  let tags = tags++"ocaml"++"native" in
  Cmd (S [!Options.ocamlopt; A"-c"; Ocaml_arch.forpack_flags_of_pathname arg;
          T(tags++"compile"); Ocaml_utils.ocaml_ppflags tags; Ocaml_utils.ocaml_include_flags arg;
          A "-pp"; P reasonfmt;
          A"-o"; Px out (* FIXME ocamlopt bug -o cannot be after the input file *);
          if impl then A"-impl" else A"-intf-suffix"; P arg])

let native_compile_re_implem ?tag ?(cmx_ext="cmx") re env build =
  let re = env re in
  let cmi = Pathname.update_extensions "cmi" re in
  let cmx = Pathname.update_extensions cmx_ext re in
  Ocaml_compiler.prepare_link cmx cmi [cmx_ext; "cmi"] build;
  ocamlopt_c (Tags.union (tags_of_pathname re) (tags_of_pathname cmx)++"implem"+++tag) re cmx true

let compile_ocaml_interf rei cmi env build =
  let rei = env rei and cmi = env cmi in
  Ocaml_compiler.prepare_compile build rei;
  let tags = tags_of_pathname rei++"interf" in
  let comp_c = if Tags.mem "native" tags then ocamlopt_c else ocamlc_c in
    comp_c tags rei cmi false

let ocamldep_command ~impl arg out env _build =
  let out = List.map env out in
  let out = List.map (fun n -> Px n) out in
  let teeout = [Sh"|"; P "tee"] @ out in
  let arg = env arg in
  let tags = tags_of_pathname arg in
  let impl_str = if impl then "-impl" else "-intf" in
  Cmd(S([ocamldep_command' tags; A "-pp"; P reasonfmt; A impl_str; P arg] @ teeout));;

rule "rei -> cmi"
  ~prod:"%.cmi"
  ~deps:["%.rei"; "%.rei.depends"]
  (compile_ocaml_interf "%.rei" "%.cmi");;

rule "re dependecies"
~prods:["%.re.depends"; "%.ml.depends" (* .ml.depends is also needed since
    the function "prepare_link" requires .ml.depends *)]
   ~deps:(["%.re"])
   (ocamldep_command ~impl:true "%.re" ["%.re.depends"; "%.ml.depends"]);;

rule "rei dependencies"
  ~prods:["%.rei.depends"; "%.mli.depends"]
  ~dep:"%.rei"
  (ocamldep_command ~impl:false "%.rei" ["%.rei.depends"; "%.mli.depends"]);;

rule "re -> d.cmo & cmi"
  ~prods:["%.d.cmo"]
  ~deps:["%.re"; "%.re.depends"; "%.cmi"]
  (byte_compile_re_implem ~tag:"debug" "%.re" "%.d.cmo");;

rule "re & cmi -> cmo"
  ~prod:"%.cmo"
  ~deps:["%.rei"(* This one is inserted to force this rule to be skipped when
                   a .ml is provided without a .mli *); "%.re"; "%.re.depends"; "%.cmi"]
  (byte_compile_re_implem "%.re" "%.cmo");;

rule "re -> cmo & cmi"
   ~prods:["%.cmo"; "%.cmi"]
   ~deps:(["%.re"; "%.re.depends"])
   (byte_compile_re_implem "%.re" "%.cmo");;

rule "re & cmi -> d.cmo"
   ~prod:"%.d.cmo"
   ~deps:["%.rei"(* This one is inserted to force this rule to be skipped when
        a .re is provided without a .rei *); "%.re"; "%.re.depends"; "%.cmi"]
   (byte_compile_re_implem ~tag:"debug" "%.re" "%.d.cmo");;

rule "re & rei -> cmx & o"
   ~prods:["%.cmx"; x_o]
   ~deps:["%.re"; "%.ml.depends"; "%.cmi"]
   (native_compile_re_implem "%.re");;
