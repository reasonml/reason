(*
 *  Copyright (c) 2015-present, Facebook, Inc.
 *  All rights reserved.
 *
 *  This source code is licensed under the BSD-style license found in the
 *  LICENSE file in the root directory of this source tree. An additional grant
 *  of patent rights can be found in the PATENTS file in the same directory.
 *
 *  Forked from OCaml, which is provided under the license below:
 *
 *  Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *
 *  Copyright © 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006 Inria
 *
 *  Permission is hereby granted, free of charge, to the Licensee obtaining a
 *  copy of this software and associated documentation files (the "Software"),
 *  to deal in the Software without restriction, including without limitation
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense
 *  under any license of the Licensee's choice, and/or sell copies of the
 *  Software, subject to the following conditions:
 *
 *  1.	Redistributions of source code must retain the above copyright notice
 *  and the following disclaimer.
 *  2.	Redistributions in binary form must reproduce the above copyright
 *  notice, the following disclaimer in the documentation and/or other
 *  materials provided with the distribution.
 *  3.	All advertising materials mentioning features or use of the Software
 *  must display the following acknowledgement: This product includes all or
 *  parts of the Caml system developed by Inria and its contributors.
 *  4.	Other than specified in clause 3, neither the name of Inria nor the
 *  names of its contributors may be used to endorse or promote products
 *  derived from the Software without specific prior written permission.
 *
 *  Disclaimer
 *
 *  This software is provided by Inria and contributors “as is” and any express
 *  or implied warranties, including, but not limited to, the implied
 *  warranties of merchantability and fitness for a particular purpose are
 *  disclaimed. in no event shall Inria or its contributors be liable for any
 *  direct, indirect, incidental, special, exemplary, or consequential damages
 *  (including, but not limited to, procurement of substitute goods or
 *  services; loss of use, data, or profits; or business interruption) however
 *  caused and on any theory of liability, whether in contract, strict
 *  liability, or tort (including negligence or otherwise) arising in any way
 *  out of the use of this software, even if advised of the possibility of such
 *  damage.
 *
 *)

open Odoc_info
module Naming = Odoc_html.Naming
open Odoc_info.Value
open Odoc_info.Module
open Odoc_info.Extension
open Odoc_info.Exception
open Odoc_info.Type
open Odoc_info.Class

let p = Printf.bprintf
let bp = Printf.bprintf
let bs = Buffer.add_string

let _ = Oprint.out_value := Reason_oprint.print_out_value
let _ = Oprint.out_type := Reason_oprint.print_out_type
let _ = Oprint.out_class_type := Reason_oprint.print_out_class_type
let _ = Oprint.out_module_type := Reason_oprint.print_out_module_type
let _ = Oprint.out_sig_item := Reason_oprint.print_out_sig_item
let _ = Oprint.out_signature := Reason_oprint.print_out_signature
let _ = Oprint.out_type_extension := Reason_oprint.print_out_type_extension
let _ = Oprint.out_phrase := Reason_oprint.print_out_phrase

module Html =
  (val
    (
      match !Odoc_args.current_generator with
        None -> (module Odoc_html.Generator : Odoc_html.Html_generator)
      | Some (Odoc_gen.Html m) -> m
      | _ ->
        failwith
          "A non-html generator is already set. Cannot install the Todo-list html generator"
    ) : Odoc_html.Html_generator)
;;

let raw_string_of_type_list sep type_list =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  let rec need_parent t =
    match t.Types.desc with
      Types.Tarrow _ | Types.Ttuple _ -> true
    | Types.Tlink t2 | Types.Tsubst t2 -> need_parent t2
    | Types.Tconstr _ ->
      false
    | Types.Tvar _ | Types.Tunivar _ | Types.Tobject _ | Types.Tpoly _
    | Types.Tfield _ | Types.Tnil | Types.Tvariant _ | Types.Tpackage _ -> false
  in
  let print_one_type variance t =
    Printtyp.mark_loops t;
    if need_parent t then
      (
        Format.fprintf fmt "(%s" variance;
        Printtyp.type_scheme_max ~b_reset_names: false fmt t;
        Format.fprintf fmt ")"
      )
    else
      (
        Format.fprintf fmt "%s" variance;
        Printtyp.type_scheme_max ~b_reset_names: false fmt t
      )
  in
  begin match type_list with
      [] -> ()
    | [(variance, ty)] -> print_one_type variance ty
    | (variance, ty) :: tyl ->
      Format.fprintf fmt "@[<hov 2>";
      print_one_type variance ty;
      List.iter
        (fun (variance, t) ->
           Format.fprintf fmt "@,%s" sep;
           print_one_type variance t
        )
        tyl;
      Format.fprintf fmt "@]"
  end;
  Format.pp_print_flush fmt ();
  Buffer.contents buf


let string_of_type_param_list t =
  Printf.sprintf "%s"
    (raw_string_of_type_list " "
       (List.map
          (fun (typ, co, cn) -> (Odoc_str.string_of_variance t (co, cn), typ))
          t.Odoc_type.ty_parameters
       )
    )

let string_of_type_extension_param_list te =
  Printf.sprintf "%s"
    (raw_string_of_type_list " "
       (List.map
          (fun typ -> ("", typ))
          te.Odoc_extension.te_type_parameters
       )
    )

let string_of_value v =
  let module M = Odoc_value in
  "let "^(Name.simple v.M.val_name)^" : "^
  (Odoc_print.string_of_type_expr v.M.val_type)^"\n"^
  (match v.M.val_info with
     None -> ""
   | Some i -> Odoc_misc.string_of_info i)

module Generator =
struct
  class html =
    object (self)
      inherit Html.html as html

      method html_of_type_expr_param_list b m_name t =
        let s = string_of_type_param_list t in
        let s2 = Odoc_html.newline_to_indented_br s in
        bs b "<code class=\"type\">";
        bs b (self#create_fully_qualified_idents_links m_name s2);
        bs b "</code>"

      method html_of_module_kind b father ?modu kind =
        match kind with
          Module_struct eles ->
          self#html_of_text b [Code "{"];
          (
            match modu with
              None ->
              bs b "<div class=\"sig_block\">";
              List.iter (self#html_of_module_element b father) eles;
              bs b "</div>"
            | Some m ->
              let (html_file, _) = Naming.html_files m.m_name in
              bp b " <a href=\"%s\">..</a> " html_file
          );
          self#html_of_text b [Code "}"]
        | _ -> html#html_of_module_kind b father ?modu kind

      method html_of_module_parameter b father p =
        let (s_functor,s_arrow) =
          if !Odoc_html.html_short_functors then
            "", ""
          else
            "", "=> "
        in
        self#html_of_text b
          [
            Code (s_functor^"(");
            Code p.mp_name ;
            Code " : ";
          ] ;
        self#html_of_module_type_kind b father p.mp_kind;
        self#html_of_text b [ Code (") "^s_arrow)]

      method html_of_module_type_kind b father ?modu ?mt kind =
        match kind with
          Module_type_struct eles ->
          self#html_of_text b [Code "{"];
          (
            match mt with
              None ->
              (
                match modu with
                  None ->
                  bs b "<div class=\"sig_block\">";
                  List.iter (self#html_of_module_element b father) eles;
                  bs b "</div>"
                | Some m ->
                  let (html_file, _) = Naming.html_files m.m_name in
                  bp b " <a href=\"%s\">..</a> " html_file
              )
            | Some mt ->
              let (html_file, _) = Naming.html_files mt.mt_name in
              bp b " <a href=\"%s\">..</a> " html_file
          );
          self#html_of_text b [Code "}"]
        | _ -> html#html_of_module_type_kind b father ?modu ?mt kind

      method html_of_value b v =
        Odoc_info.reset_type_names ();
        bs b "\n<pre>" ;
        bp b "<span id=\"%s\">" (Naming.value_target v);
        bs b (self#keyword "let");
        bs b " ";
        (
          match v.val_code with
            None -> bs b (self#escape (Name.simple v.val_name))
          | Some c ->
            let file = Naming.file_code_value_complete_target v in
            self#output_code v.val_name (Filename.concat !Global.target_dir file) c;
            bp b "<a href=\"%s\">%s</a>" file (self#escape (Name.simple v.val_name))
        );
        bs b "</span>";
        bs b " : ";
        self#html_of_type_expr b (Name.father v.val_name) v.val_type;
        bs b "</pre>";
        self#html_of_info b v.val_info;
        (
          if !Odoc_html.with_parameter_list then
            self#html_of_parameter_list b (Name.father v.val_name) v.val_parameters
          else
            self#html_of_described_parameter_list b (Name.father v.val_name) v.val_parameters
        )

      method html_of_type_extension b m_name te =
        Odoc_info.reset_type_names ();
        bs b "<pre><code>";
        bs b ((self#keyword "type")^" ");
        let s = string_of_type_extension_param_list te in
        let s2 = Odoc_html.newline_to_indented_br s in
        bs b "<code class=\"type\">";
        bs b (self#create_fully_qualified_idents_links m_name s2);
        bs b "</code>";
        (match te.te_type_parameters with [] -> () | _ -> bs b " ");
        bs b (self#create_fully_qualified_idents_links m_name te.te_type_name);
        bs b " += ";
        if te.te_private = Asttypes.Private then bs b "private ";
        bs b "</code></pre>";
        bs b "<table class=\"typetable\">\n";
        let print_one x =
          let father = Name.father x.xt_name in
          bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n";
          bs b "<code>";
          bs b (self#keyword "|");
          bs b "</code></td>\n<td align=\"left\" valign=\"top\" >\n";
          bs b "<code>";
          bp b "<span id=\"%s\">%s</span>"
            (Naming.extension_target x)
            (Name.simple x.xt_name);
          (
            match x.xt_args, x.xt_ret with
              [], None -> ()
            | l,None ->
              bs b (" " ^ (self#keyword "of") ^ " ");
              self#html_of_type_expr_list ~par: false b father " " l;
            | [],Some r ->
              bs b (" " ^ (self#keyword ":") ^ " ");
              self#html_of_type_expr b father r;
            | l,Some r ->
              bs b (" " ^ (self#keyword "of") ^ " ");
              self#html_of_type_expr_list ~par: false b father " " l;
              bs b (" " ^ (self#keyword ":") ^ " ");
              self#html_of_type_expr b father r;
          );
          (
            match x.xt_alias with
              None -> ()
            | Some xa ->
              bs b " = ";
              (
                match xa.xa_xt with
                  None -> bs b xa.xa_name
                | Some x ->
                  bp b "<a href=\"%s\">%s</a>" (Naming.complete_extension_target x) x.xt_name
              )
          );
          bs b "</code></td>\n";
          (
            match x.xt_text with
              None -> ()
            | Some t ->
              bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
              bs b "<code>";
              bs b "(*";
              bs b "</code></td>";
              bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
              self#html_of_info b (Some t);
              bs b "</td>";
              bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >";
              bs b "<code>";
              bs b "*)";
              bs b "</code></td>";
          );
          bs b "\n</tr>"
        in
        Odoc_html.print_concat b "\n" print_one te.te_constructors;
        bs b "</table>\n";
        bs b "\n";
        self#html_of_info b te.te_info;
        bs b "\n"

      method html_of_exception b e =
        Odoc_info.reset_type_names ();
        bs b "\n<pre>";
        bp b "<span id=\"%s\">" (Naming.exception_target e);
        bs b (self#keyword "exception");
        bs b " ";
        bs b (Name.simple e.ex_name);
        bs b "</span>";
        (
          match e.ex_args, e.ex_ret with
            [], None -> ()
          | l,None ->
            bs b (" "^(self#keyword "of")^" ");
            self#html_of_type_expr_list
              ~par: false b (Name.father e.ex_name) " " e.ex_args
          | [],Some r ->
            bs b (" " ^ (self#keyword ":") ^ " ");
            self#html_of_type_expr b (Name.father e.ex_name) r;
          | l,Some r ->
            bs b (" " ^ (self#keyword "of") ^ " ");
            self#html_of_type_expr_list
              ~par: false b (Name.father e.ex_name) " " l;
            bs b (" " ^ (self#keyword ":") ^ " ");
            self#html_of_type_expr b (Name.father e.ex_name) r;
        );
        (
          match e.ex_alias with
            None -> ()
          | Some ea ->
            bs b " = ";
            (
              match ea.ea_ex with
                None -> bs b ea.ea_name
              | Some e ->
                bp b "<a href=\"%s\">%s</a>" (Naming.complete_exception_target e) e.ex_name
            )
        );
        bs b "</pre>\n";
        self#html_of_info b e.ex_info

      method html_of_type b t =
        Odoc_info.reset_type_names ();
        let father = Name.father t.ty_name in
        let print_field_prefix () =
          bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n";
          bs b "<code>&nbsp;&nbsp;</code>";
          bs b "</td>\n<td align=\"left\" valign=\"top\" >\n";
          bs b "<code>";
        in
        let print_field_comment = function
          | None -> ()
          | Some t ->
            bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
            bs b "<code>";
            bs b "(*";
            bs b "</code></td>";
            bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
            self#html_of_info b (Some t);
            bs b "</td><td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >";
            bs b "<code>*)</code></td>"
        in
        bs b
          (match t.ty_manifest, t.ty_kind with
             None, Type_abstract
           | None, Type_open -> "\n<pre>"
           | None, Type_variant _
           | None, Type_record _ -> "\n<pre><code>"
           | Some _, Type_abstract
           | Some _, Type_open -> "\n<pre>"
           | Some _, Type_variant _
           | Some _, Type_record _ -> "\n<pre>"
          );
        bp b "<span id=\"%s\">" (Naming.type_target t);
        bs b ((self#keyword "type")^" ");
        bs b (Name.simple t.ty_name);
        (match t.ty_parameters with [] -> () | _ -> bs b " ");
        self#html_of_type_expr_param_list b father t;
        bs b "</span> ";
        let priv = t.ty_private = Asttypes.Private in
        (
          match t.ty_manifest with
            None -> ()
          | Some (Object_type fields) ->
            bs b "= ";
            if priv then bs b "private ";
            bs b "&lt;</pre>";
            bs b "<table class=\"typetable\">\n" ;
            let print_one f =
              print_field_prefix () ;
              bp b "<span id=\"%s\">%s</span>&nbsp;: "
                (Naming.objfield_target t f)
                f.of_name;
              self#html_of_type_expr b father f.of_type;
              bs b ";</code></td>\n";
              print_field_comment f.of_text ;
              bs b "\n</tr>"
            in
            Odoc_html.print_concat b "\n" print_one fields;
            bs b "</table>\n>\n";
            bs b " "
          | Some (Other typ) ->
            bs b "= ";
            if priv then bs b "private ";
            self#html_of_type_expr b father typ;
            bs b " "
        );
        (match t.ty_kind with
           Type_abstract -> bs b "</pre>"
         | Type_variant l ->
           bs b "= ";
           if priv then bs b "private ";
           bs b
             (
               match t.ty_manifest with
                 None -> "</code></pre>"
               | Some _ -> "</pre>"
             );
           bs b "<table class=\"typetable\">\n";
           let print_one constr =
             bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n";
             bs b "<code>";
             bs b (self#keyword "|");
             bs b "</code></td>\n<td align=\"left\" valign=\"top\" >\n";
             bs b "<code>";
             bp b "<span id=\"%s\">%s</span>"
               (Naming.const_target t constr)
               (self#constructor constr.vc_name);
             (
               match constr.vc_args, constr.vc_ret with
                 [], None -> ()
               | l,None ->
                 bs b (" " ^ (self#keyword "of") ^ " ");
                 self#html_of_type_expr_list ~par: false b father " " l;
               | [],Some r ->
                 bs b (" " ^ (self#keyword ":") ^ " ");
                 self#html_of_type_expr b father r;
               | l,Some r ->
                 bs b (" " ^ (self#keyword "of") ^ " ");
                 self#html_of_type_expr_list ~par: false b father " " l;
                 bs b (" " ^ (self#keyword ":") ^ " ");
                 self#html_of_type_expr b father r;
             );
             bs b "</code></td>\n";
             (
               match constr.vc_text with
                 None -> ()
               | Some t ->
                 bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                 bs b "<code>";
                 bs b "(*";
                 bs b "</code></td>";
                 bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                 self#html_of_info b (Some t);
                 bs b "</td>";
                 bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >";
                 bs b "<code>";
                 bs b "*)";
                 bs b "</code></td>";
             );
             bs b "\n</tr>"
           in
           Odoc_html.print_concat b "\n" print_one l;
           bs b "</table>\n"
         | Type_record l ->
           bs b "= ";
           if priv then bs b "private " ;
           bs b "{";
           bs b
             (
               match t.ty_manifest with
                 None -> "</code></pre>"
               | Some _ -> "</pre>"
             );
           bs b "<table class=\"typetable\">\n" ;
           let print_one r =
             bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n";
             bs b "<code>&nbsp;&nbsp;</code>";
             bs b "</td>\n<td align=\"left\" valign=\"top\" >\n";
             bs b "<code>";
             if r.rf_mutable then bs b (self#keyword "mutable&nbsp;") ;
             bp b "<span id=\"%s\">%s</span>&nbsp;: "
               (Naming.recfield_target t r)
               r.rf_name;
             self#html_of_type_expr b father r.rf_type;
             bs b ",</code></td>\n";
             (
               match r.rf_text with
                 None -> ()
               | Some t ->
                 bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                 bs b "<code>";
                 bs b "(*";
                 bs b "</code></td>";
                 bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
                 self#html_of_info b (Some t);
                 bs b "</td><td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >";
                 bs b "<code>*)</code></td>";
             );
             bs b "\n</tr>"
           in
           Odoc_html.print_concat b "\n" print_one l;
           bs b "</table>\n}\n"
         | Type_open ->
           bs b "= ..";
           bs b "</pre>"
        );
        bs b "\n";
        self#html_of_info b t.ty_info;
        bs b "\n"

      method html_of_class_kind b father ?cl kind =
        match kind with
          Class_structure (inh, eles) ->
          self#html_of_text b [Code "{"];
          (
            match cl with
              None ->
              bs b "\n";
              (
                match inh with
                  [] -> ()
                | _ ->
                  self#generate_inheritance_info b inh
              );
              List.iter (self#html_of_class_element b) eles;
            | Some cl ->
              let (html_file, _) = Naming.html_files cl.cl_name in
              bp b " <a href=\"%s\">..</a> " html_file
          );
          self#html_of_text b [Code "}"]
        | _ -> html#html_of_class_kind b father ?cl kind


      method html_of_class_type_kind b father ?ct kind =
        match kind with
          Class_signature (inh, eles) ->
          self#html_of_text b [Code "{"];
          (
            match ct with
              None ->
              bs b "\n";
              (
                match inh with
                  [] -> ()
                | _ -> self#generate_inheritance_info b inh
              );
              List.iter (self#html_of_class_element b) eles
            | Some ct ->
              let (html_file, _) = Naming.html_files ct.clt_name in
              bp b " <a href=\"%s\">..</a> " html_file
          );
          self#html_of_text b [Code "}"]
        | _ -> html#html_of_class_type_kind b father ?ct kind

    end
end

let _ = Odoc_args.set_generator
    (Odoc_gen.Html (module Generator : Odoc_html.Html_generator))
;;
