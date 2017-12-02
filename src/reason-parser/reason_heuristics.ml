let is_punned_labelled_expression e lbl = 
  let open Ast_404.Parsetree in
  match e.pexp_desc with
  | Pexp_ident { txt; _ }
  | Pexp_constraint ({pexp_desc = Pexp_ident { txt; _ }; _}, _)
  | Pexp_coerce ({pexp_desc = Pexp_ident { txt; _ }; _}, _, _)
    -> txt = Longident.parse lbl
  | _ -> false

let estimateLineLengthForCallback ~args ~funExpr () =
  let open Ast_404.Parsetree in
  let open Ast_404.Asttypes in
  let funLen = begin match funExpr.pexp_desc with
    | Pexp_ident ident ->
       let identList = Longident.flatten ident.txt in
       let lengthOfDots = List.length identList - 1 in
       let len = List.fold_left (fun acc curr ->
         acc + (String.length curr)) lengthOfDots identList in
       len
    | _ -> -1 
  end in
  let rec aux len = function
    | [] -> len
    | _ when len < 0 -> len
    | arg::args -> 
      begin match arg with
      | (label, ({ pexp_desc = Pexp_ident ident } as e)) ->
        let identLen = List.fold_left (fun acc curr ->
          acc + (String.length curr)
        ) len (Longident.flatten ident.txt) in
        begin match label with
        | Nolabel -> aux (len + identLen) args
        | Labelled s  when is_punned_labelled_expression e s ->   
            aux (len + identLen + 1) args
        | _ ->
         -1 
        end
      | (_, {pexp_desc = Pexp_constant (Pconst_string (str, _))}) ->
        aux (len + (String.length str)) args
      | _ -> aux (-1) args
    end
  in
  aux funLen args

