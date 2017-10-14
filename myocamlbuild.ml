open Ocamlbuild_plugin

let () = dispatch (
  function
  | Before_rules ->
    rule "myocamlbuild"
      ~prod:"_reasonbuild/_build/myocamlbuild"
      ~deps:["src/reasonbuild.ml"]
      begin fun env build ->
        (* Generate a myocamlbuild plugin based on reasonbuild.ml *)
        Cmd(S[Sh"mkdir -p _reasonbuild;";
              Sh"cd _reasonbuild;";
              Sh"cp ../src/reasonbuild.ml myocamlbuild.ml;";
              Sh"touch myocamlbuild.ml;";
              A"ocamlbuild"; A"-just-plugin"])
      end;
  | After_rules ->
    flag ["ocaml"; "pp"; "pp_byte"; "reason"] &
      A"src/reason_pp.byte";
    flag ["ocaml"; "pp"; "pp_native"; "reason"] &
      A"src/reason_pp.native";

  | _ -> ())
