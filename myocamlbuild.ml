open Ocamlbuild_plugin

let () = dispatch (
  function
  | Before_rules ->
    rule "myocamlbuild"
      ~prod:"_reasonbuild/_build/myocamlbuild"
      ~deps:["src/reasonbuild.cmx"; "src/reopt.sh"; "src/rec.sh"; "src/share.sh"]
      begin fun env build ->
        Cmd(S[Sh"mkdir -p _reasonbuild;";
              Sh"cd _reasonbuild;";
              Sh"pwd;";
              Sh"touch myocamlbuild.ml;";
              Sh"chmod +x ../src/reopt.sh;";
              Sh"chmod +x ../src/rec.sh;";
              A"ocamlbuild"; A"-just-plugin"; A"-ocamlopt";
              A"env REASON_BUILD_DIR=../../src ../../src/reopt.sh";
              A"-ocamlc";
              A"env REASON_BUILD_DIR=../../src ../../src/rec.sh" ])
      end;
  | After_rules ->
    flag ["ocaml"; "pp"; "pp_byte"; "reason"] &
      A"src/reason_pp.byte";
    flag ["ocaml"; "pp"; "pp_native"; "reason"] &
      A"src/reason_pp.native";

  | _ -> ())
