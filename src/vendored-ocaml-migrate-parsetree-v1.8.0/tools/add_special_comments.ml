(* Add (*IF_CURRENT:= Parsetree.expression *) comments to type definitions. *)

open StdLabels
open Parsetree

[@@@warning "-40"]

let read_file fn =
  let ic = open_in_bin fn in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let collect_insertions structure =
  let insertions = ref [] in
  let add_after ~(loc:Location.t) txt =
    insertions := (loc.loc_end.pos_cnum, txt) :: !insertions
  in
  List.iter structure ~f:(fun item ->
    match item.pstr_desc with
    | Pstr_module { pmb_name = module_name
                  ; pmb_expr = { pmod_desc = Pmod_structure items; _ }
                  ; _
                  } ->
      List.iter items ~f:(fun item ->
        match item.pstr_desc with
        | Pstr_type (_, tds) ->
          List.iter tds ~f:(fun td ->
            match td.ptype_manifest with
            | Some _ -> ()
            | None ->
              let name = td.ptype_name in
              let params =
                let to_string (ty, _) =
                  Format.asprintf "%a" Pprintast.core_type ty
                in
                match td.ptype_params with
                | [] -> ""
                | [param] -> to_string param ^ " "
                | l ->
                  Printf.sprintf "(%s) "
                    (String.concat ~sep:", " (List.map l ~f:to_string))
              in
              Printf.ksprintf (add_after ~loc:name.loc)
                " (*IF_CURRENT = %s%s.%s *)" params (Option.value module_name.txt ~default:"X") name.txt)
        | _ -> ())
    | _ -> ());
  List.sort !insertions ~cmp:(fun (a, _) (b, _) -> compare a b)

let () =
  let fn = Sys.argv.(1) in
  let file_contents = read_file fn in
  let lb = Lexing.from_string file_contents in
  Location.init lb fn;
  let ast = Parse.implementation lb in
  let insertions = collect_insertions ast in
  let oc = open_out_bin fn in
  let pos =
    List.fold_left insertions ~init:0 ~f:(fun cur_pos (pos, txt) ->
      output_substring oc file_contents cur_pos (pos - cur_pos);
      output_string oc txt;
      pos)
  in
  output_substring oc file_contents pos (String.length file_contents - pos);
  close_out oc
