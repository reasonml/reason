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
(** Generation of html documentation.*)

(*
let print_DEBUG s = print_string s ; print_newline ()

open Odoc_info
open Value
open Type
open Extension
open Exception
open Class
open Module

let with_parameter_list = ref false
let css_style = ref None
let index_only = ref false
let colorize_code = ref false
let html_short_functors = ref false
let charset = ref "iso-8859-1"


(** The functions used for naming files and html marks.*)
module Naming =
  struct
    (** The prefix for types marks. *)
    let mark_type = "TYPE"

    (** The prefix for types elements (record fields or constructors). *)
    let mark_type_elt = "TYPEELT"

    (** The prefix for functions marks. *)
    let mark_function = "FUN"

    (** The prefix for extensions marks. *)
    let mark_extension = "EXTENSION"

    (** The prefix for exceptions marks. *)
    let mark_exception = "EXCEPTION"

    (** The prefix for values marks. *)
    let mark_value = "VAL"

    (** The prefix for attributes marks. *)
    let mark_attribute = "ATT"

    (** The prefix for methods marks. *)
    let mark_method = "METHOD"

    (** The prefix for code files.. *)
    let code_prefix = "code_"

    (** The prefix for type files.. *)
    let type_prefix = "type_"

    (** Return the two html files names for the given module or class name.*)
    let html_files name =
      let qual =
        try
          let i = String.rindex name '.' in
          match name.[i + 1] with
          | 'A'..'Z' -> ""
          | _ -> "-c"
        with Not_found -> ""
      in
      let prefix = name^qual in
      let html_file = prefix^".html" in
      let html_frame_file = prefix^"-frame.html" in
      (html_file, html_frame_file)

    (** Return the target for the given prefix and simple name. *)
    let target pref simple_name = pref^simple_name

    (** Return the complete link target (file#target) for the given prefix string and complete name.*)
    let complete_target pref complete_name =
      let simple_name = Name.simple complete_name in
      let module_name =
        let s = Name.father complete_name in
        if s = "" then simple_name else s
      in
      let (html_file, _) = html_files module_name in
      html_file^"#"^(target pref simple_name)

    (** Return the link target for the given type. *)
    let type_target t = target mark_type (Name.simple t.ty_name)

    (** Return the link target for the given variant constructor. *)
    let const_target t f =
      let name = Printf.sprintf "%s.%s" (Name.simple t.ty_name) f.vc_name in
      target mark_type_elt name

    (** Return the link target for the given record field. *)
    let recfield_target t f = target mark_type_elt
      (Printf.sprintf "%s.%s" (Name.simple t.ty_name) f.rf_name)

    (** Return the link target for the given object field. *)
    let objfield_target t f = target mark_type_elt
      (Printf.sprintf "%s.%s" (Name.simple t.ty_name) f.of_name)

    (** Return the complete link target for the given type. *)
    let complete_type_target t = complete_target mark_type t.ty_name

    let complete_recfield_target name =
      let typ = Name.father name in
      let field = Name.simple name in
      Printf.sprintf "%s.%s" (complete_target mark_type_elt typ) field

    let complete_const_target = complete_recfield_target

    (** Return the link target for the given extension. *)
    let extension_target x = target mark_extension (Name.simple x.xt_name)

    (** Return the complete link target for the given extension. *)
    let complete_extension_target x = complete_target mark_extension x.xt_name

    (** Return the link target for the given exception. *)
    let exception_target e = target mark_exception (Name.simple e.ex_name)

    (** Return the complete link target for the given exception. *)
    let complete_exception_target e = complete_target mark_exception e.ex_name

    (** Return the link target for the given value. *)
    let value_target v = target mark_value (Name.simple v.val_name)

    (** Return the given value name where symbols accepted in infix values
       are replaced by strings, to avoid clashes with the filesystem.*)
    let subst_infix_symbols name =
      let len = String.length name in
      let buf = Buffer.create len in
      let ch c = Buffer.add_char buf c in
      let st s = Buffer.add_string buf s in
      for i = 0 to len - 1 do
        match name.[i] with
        | '|' -> st "_pipe_"
        | '<' -> st "_lt_"
        | '>' -> st "_gt_"
        | '@' -> st "_at_"
        | '^' -> st "_exp_"
        | '&' -> st "_amp_"
        | '+' -> st "_plus_"
        | '-' -> st "_minus_"
        | '*' -> st "_star_"
        | '/' -> st "_slash_"
        | '$' -> st "_dollar_"
        | '%' -> st "_percent_"
        | '=' -> st "_equal_"
        | ':' -> st "_column_"
        | '~' -> st "_tilde_"
        | '!' -> st "_bang_"
        | '?' -> st "_questionmark_"
        | c -> ch c
      done;
      Buffer.contents buf

    (** Return the complete link target for the given value. *)
    let complete_value_target v = complete_target mark_value v.val_name

    (** Return the complete filename for the code of the given value. *)
    let file_code_value_complete_target v =
      code_prefix^mark_value^(subst_infix_symbols v.val_name)^".html"

    (** Return the link target for the given attribute. *)
    let attribute_target a = target mark_attribute (Name.simple a.att_value.val_name)

    (** Return the complete link target for the given attribute. *)
    let complete_attribute_target a = complete_target mark_attribute a.att_value.val_name

    (** Return the complete filename for the code of the given attribute. *)
    let file_code_attribute_complete_target a =
      code_prefix^mark_attribute^a.att_value.val_name^".html"

    (** Return the link target for the given method. *)
    let method_target m = target mark_method (Name.simple m.met_value.val_name)

    (** Return the complete link target for the given method. *)
    let complete_method_target m = complete_target mark_method m.met_value.val_name

    (** Return the complete filename for the code of the given method. *)
    let file_code_method_complete_target m =
      code_prefix^mark_method^m.met_value.val_name^".html"

    (** Return the link target for the given label section. *)
    let label_target l = target "" l

    (** Return the complete link target for the given section label. *)
    let complete_label_target l = complete_target "" l

    (** Return the complete filename for the code of the type of the
       given module or module type name. *)
    let file_type_module_complete_target name =
      type_prefix^name^".html"

    (** Return the complete filename for the code of the
       given module name. *)
    let file_code_module_complete_target name =
      code_prefix^name^".html"

    (** Return the complete filename for the code of the type of the
       given class or class type name. *)
    let file_type_class_complete_target name =
      type_prefix^name^".html"
  end

module StringSet = Set.Make (struct
  type t = string
  let compare (x:t) y = compare x y
end)

(** A class with a method to colorize a string which represents OCaml code. *)
class ocaml_code =
  object
    method html_of_code b ?(with_pre=true) code =
      Odoc_ocamlhtml.html_of_code b ~with_pre: with_pre code
  end

let new_buf () = Buffer.create 1024
let bp = Printf.bprintf
let bs = Buffer.add_string


(** Generation of html code from text structures. *)
class virtual text =
  object (self)
    (** We want to display colorized code. *)
    inherit ocaml_code

    (** Escape the strings which would clash with html syntax, and
       make some replacements (double newlines replaced by <br>). *)
    method escape s = Odoc_ocamlhtml.escape_base s

    method keep_alpha_num s =
      let len = String.length s in
      let buf = Buffer.create len in
      for i = 0 to len - 1 do
        match s.[i] with
          'a'..'z' | 'A'..'Z' | '0'..'9' -> Buffer.add_char buf s.[i]
        | _ -> ()
      done;
      Buffer.contents buf

    (** Return a label created from the first sentence of a text. *)
    method label_of_text t=
      let t2 = Odoc_info.first_sentence_of_text t in
      let s = Odoc_info.string_of_text t2 in
      self#keep_alpha_num s

    (** Create a label for the associated title.
       Return the label specified by the user or a label created
       from the title level and the first sentence of the title. *)
    method create_title_label (n,label_opt,t) =
      match label_opt with
        Some s -> s
      | None -> Printf.sprintf "%d_%s" n (self#label_of_text t)

    (** Print the html code corresponding to the [text] parameter. *)
    method html_of_text b t =
      List.iter (self#html_of_text_element b) t

    (** Print the html code for the [text_element] in parameter. *)
    method html_of_text_element b txt =
      print_DEBUG "text::html_of_text_element";
      match txt with
      | Odoc_info.Raw s -> self#html_of_Raw b s
      | Odoc_info.Code s -> self#html_of_Code b s
      | Odoc_info.CodePre s -> self#html_of_CodePre b s
      | Odoc_info.Verbatim s -> self#html_of_Verbatim b s
      | Odoc_info.Bold t -> self#html_of_Bold b t
      | Odoc_info.Italic t -> self#html_of_Italic b t
      | Odoc_info.Emphasize t -> self#html_of_Emphasize b t
      | Odoc_info.Center t -> self#html_of_Center b t
      | Odoc_info.Left t -> self#html_of_Left b t
      | Odoc_info.Right t -> self#html_of_Right b t
      | Odoc_info.List tl -> self#html_of_List b tl
      | Odoc_info.Enum tl -> self#html_of_Enum b tl
      | Odoc_info.Newline -> self#html_of_Newline b
      | Odoc_info.Block t -> self#html_of_Block b t
      | Odoc_info.Title (n, l_opt, t) -> self#html_of_Title b n l_opt t
      | Odoc_info.Latex s -> self#html_of_Latex b s
      | Odoc_info.Link (s, t) -> self#html_of_Link b s t
      | Odoc_info.Ref (name, ref_opt, text_opt) ->
          self#html_of_Ref b name ref_opt text_opt
      | Odoc_info.Superscript t -> self#html_of_Superscript b t
      | Odoc_info.Subscript t -> self#html_of_Subscript b t
      | Odoc_info.Module_list l -> self#html_of_Module_list b l
      | Odoc_info.Index_list -> self#html_of_Index_list b
      | Odoc_info.Custom (s,t) -> self#html_of_custom_text b s t
      | Odoc_info.Target (target, code) -> self#html_of_Target b ~target ~code

    method html_of_custom_text _ _ _ = ()

    method html_of_Target b ~target ~code =
      if String.lowercase_ascii target = "html" then bs b code else ()

    method html_of_Raw b s = bs b (self#escape s)

    method html_of_Code b s =
      if !colorize_code then
        self#html_of_code b ~with_pre: false s
      else
        (
         bs b "<code class=\"";
         bs b Odoc_ocamlhtml.code_class ;
         bs b "\">";
         bs b (self#escape s);
         bs b "</code>"
        )

    method html_of_CodePre =
        let remove_useless_newlines s =
          let len = String.length s in
          let rec iter_first n =
            if n >= len then
              None
            else
              match s.[n] with
              | '\n' -> iter_first (n+1)
              | _ -> Some n
          in
          match iter_first 0 with
            None -> ""
          | Some first ->
              let rec iter_last n =
                if n <= first then
                  None
                else
                  match s.[n] with
                    '\t'  -> iter_last (n-1)
                  | _ -> Some n
              in
              match iter_last (len-1) with
                None -> String.sub s first 1
              | Some last -> String.sub s first ((last-first)+1)
        in
        fun b s ->
      if !colorize_code then
          (
           bs b "<pre class=\"codepre\">";
           self#html_of_code b (remove_useless_newlines s);
           bs b "</pre>"
          )
      else
        (
         bs b "<pre class=\"codepre\"><code class=\"";
         bs b Odoc_ocamlhtml.code_class;
         bs b "\">" ;
         bs b (self#escape (remove_useless_newlines s));
         bs b "</code></pre>"
        )

    method html_of_Verbatim b s =
      bs b "<pre class=\"verbatim\">";
      bs b (self#escape s);
      bs b "</pre>"

    method html_of_Bold b t =
      bs b "<b>";
      self#html_of_text b t;
      bs b "</b>"

    method html_of_Italic b t =
      bs b "<i>" ;
      self#html_of_text b t;
      bs b "</i>"

    method html_of_Emphasize b t =
      bs b "<em>" ;
      self#html_of_text b t ;
      bs b "</em>"

    method html_of_Center b t =
      bs b "<center>";
      self#html_of_text b t;
      bs b "</center>"

    method html_of_Left b t =
      bs b "<div align=left>";
      self#html_of_text b t;
      bs b "</div>"

    method html_of_Right b t =
      bs b "<div align=right>";
      self#html_of_text b t;
      bs b "</div>"

    method html_of_List b tl =
      bs b "<ul>\n";
      List.iter
        (fun t -> bs b "<li>"; self#html_of_text b t; bs b "</li>\n")
        tl;
      bs b "</ul>\n"

    method html_of_Enum b tl =
      bs b "<OL>\n";
      List.iter
        (fun t -> bs b "<li>"; self#html_of_text b t; bs b"</li>\n")
        tl;
      bs b "</OL>\n"

    method html_of_Newline b = bs b "\n<p>\n"

    method html_of_Block b t =
      bs b "<blockquote>\n";
      self#html_of_text b t;
      bs b "</blockquote>\n"

    method html_of_Title b n label_opt t =
      let label1 = self#create_title_label (n, label_opt, t) in
      let (tag_o, tag_c) =
        if n > 6 then
          (Printf.sprintf "div class=\"h%d\"" n, "div")
        else
          let t = Printf.sprintf "h%d" n in (t, t)
      in
      bs b "<";
      bp b "%s id=\"%s\"" tag_o (Naming.label_target label1);
      bs b ">";
      self#html_of_text b t;
      bs b "</";
      bs b tag_c;
      bs b ">"

    method html_of_Latex _ _ = ()
      (* don't care about LaTeX stuff in HTML. *)

    method html_of_Link b s t =
      bs b "<a href=\"";
      bs b s ;
      bs b "\">";
      self#html_of_text b t;
      bs b "</a>"

    method html_of_Ref b name ref_opt text_opt =
      match ref_opt with
        None ->
          let text =
            match text_opt with
              None -> [Odoc_info.Code name]
            | Some t -> t
          in
          self#html_of_text b text
      | Some kind ->
          let h name = Odoc_info.Code (Odoc_info.use_hidden_modules name) in
          let (target, text) =
            match kind with
              Odoc_info.RK_module
            | Odoc_info.RK_module_type
            | Odoc_info.RK_class
            | Odoc_info.RK_class_type ->
                let (html_file, _) = Naming.html_files name in
                (html_file, h name)
            | Odoc_info.RK_value -> (Naming.complete_target Naming.mark_value name, h name)
            | Odoc_info.RK_type -> (Naming.complete_target Naming.mark_type name, h name)
            | Odoc_info.RK_extension -> (Naming.complete_target Naming.mark_extension name, h name)
            | Odoc_info.RK_exception -> (Naming.complete_target Naming.mark_exception name, h name)
            | Odoc_info.RK_attribute -> (Naming.complete_target Naming.mark_attribute name, h name)
            | Odoc_info.RK_method -> (Naming.complete_target Naming.mark_method name, h name)
            | Odoc_info.RK_section t -> (Naming.complete_label_target name,
                                         Odoc_info.Italic [Raw (Odoc_info.string_of_text t)])
            | Odoc_info.RK_recfield -> (Naming.complete_recfield_target name, h name)
            | Odoc_info.RK_const -> (Naming.complete_const_target name, h name)
          in
          let text =
            match text_opt with
              None -> [text]
            | Some text -> text
          in
          bs b ("<a href=\""^target^"\">");
          self#html_of_text b text;
          bs b "</a>"

    method html_of_Superscript b t =
      bs b "<sup class=\"superscript\">";
      self#html_of_text b t;
      bs b "</sup>"

    method html_of_Subscript b t =
      bs b "<sub class=\"subscript\">";
      self#html_of_text b t;
      bs b "</sub>"

    method virtual html_of_info_first_sentence : _

    method html_of_Module_list b l =
      bs b "<br>\n<table class=\"indextable\">\n";
      List.iter
        (fun name ->
          bs b "<tr><td class=\"module\">";
          (
           try
             let m =
               List.find (fun m -> m.m_name = name) self#list_modules
             in
             let (html, _) = Naming.html_files m.m_name in
             bp b "<a href=\"%s\">%s</a></td>" html m.m_name;
             bs b "<td>";
             self#html_of_info_first_sentence b m.m_info;
           with
             Not_found ->
               Odoc_global.pwarning (Odoc_messages.cross_module_not_found name);
               bp b "%s</td><td>" name
          );
          bs b "</td></tr>\n"
        )
        l;
      bs b "</table>\n"

    method html_of_Index_list b =
      let index_if_not_empty l url m =
        match l with
          [] -> ()
        | _ -> bp b "<li><a href=\"%s\">%s</a></li>\n" url m
      in
      bp b "<ul class=\"indexlist\">\n";
      index_if_not_empty self#list_types self#index_types Odoc_messages.index_of_types;
      index_if_not_empty self#list_extensions self#index_extensions Odoc_messages.index_of_extensions;
      index_if_not_empty self#list_exceptions self#index_exceptions Odoc_messages.index_of_exceptions;
      index_if_not_empty self#list_values self#index_values Odoc_messages.index_of_values;
      index_if_not_empty self#list_attributes self#index_attributes Odoc_messages.index_of_attributes;
      index_if_not_empty self#list_methods self#index_methods Odoc_messages.index_of_methods;
      index_if_not_empty self#list_classes self#index_classes Odoc_messages.index_of_classes;
      index_if_not_empty self#list_class_types self#index_class_types Odoc_messages.index_of_class_types;
      index_if_not_empty self#list_modules self#index_modules Odoc_messages.index_of_modules;
      index_if_not_empty self#list_module_types self#index_module_types Odoc_messages.index_of_module_types;
      bp b "</ul>\n"

    method virtual list_types : Odoc_info.Type.t_type list
    method virtual index_types : string
    method virtual list_extensions : Odoc_info.Extension.t_extension_constructor list
    method virtual index_extensions : string
    method virtual list_exceptions : Odoc_info.Exception.t_exception list
    method virtual index_exceptions : string
    method virtual list_values : Odoc_info.Value.t_value list
    method virtual index_values : string
    method virtual list_attributes : Odoc_info.Value.t_attribute list
    method virtual index_attributes : string
    method virtual list_methods : Odoc_info.Value.t_method list
    method virtual index_methods : string
    method virtual list_classes : Odoc_info.Class.t_class list
    method virtual index_classes : string
    method virtual list_class_types : Odoc_info.Class.t_class_type list
    method virtual index_class_types : string
    method virtual list_modules : Odoc_info.Module.t_module list
    method virtual index_modules : string
    method virtual list_module_types : Odoc_info.Module.t_module_type list
    method virtual index_module_types : string

  end

(** A class used to generate html code for info structures. *)
class virtual info =
  object (self)
    (** The list of pairs [(tag, f)] where [f] is a function taking
       the [text] associated to [tag] and returning html code.
       Add a pair here to handle a tag.*)
    val mutable tag_functions = ([] : (string * (Odoc_info.text -> string)) list)

    (** The method used to get html code from a [text]. *)
    method virtual html_of_text : Buffer.t -> Odoc_info.text -> unit

    (** Print html for an author list. *)
    method html_of_author_list b l =
      match l with
        [] -> ()
      | _ ->
          bp b "<b>%s:</b> " Odoc_messages.authors;
          self#html_of_text b [Raw (String.concat ", " l)];
          bs b "<br>\n"

    (** Print html code for the given optional version information.*)
    method html_of_version_opt b v_opt =
      match v_opt with
        None -> ()
      | Some v ->
           bp b "<b>%s:</b> " Odoc_messages.version;
           self#html_of_text b [Raw v];
           bs b "<br>\n"

    (** Print html code for the given optional since information.*)
    method html_of_since_opt b s_opt =
      match s_opt with
        None -> ()
      | Some s ->
          bp b "<b>%s</b> " Odoc_messages.since;
          self#html_of_text b [Raw s];
          bs b "<br>\n"

    (** Print html code for the given "before" information.*)
    method html_of_before b l =
      let f (v, text) =
        bp b "<b>%s " Odoc_messages.before;
        self#html_of_text b [Raw v];
        bs b " </b> ";
        self#html_of_text b text;
        bs b "<br>\n"
      in
      List.iter f l

    (** Print html code for the given list of raised exceptions.*)
    method html_of_raised_exceptions b l =
      match l with
        [] -> ()
      | (s, t) :: [] ->
          bp b "<b>%s</b> <code>%s</code> "
            Odoc_messages.raises
            s;
          self#html_of_text b t;
          bs b "<br>\n"
      | _ ->
          bp b "<b>%s</b><ul>" Odoc_messages.raises;
          List.iter
            (fun (ex, desc) ->
              bp b "<li><code>%s</code> " ex ;
              self#html_of_text b desc;
              bs b "</li>\n"
            )
            l;
          bs b "</ul>\n"

    (** Print html code for the given "see also" reference. *)
    method html_of_see b (see_ref, t)  =
      let t_ref =
        match see_ref with
          Odoc_info.See_url s -> [ Odoc_info.Link (s, t) ]
        | Odoc_info.See_file s -> (Odoc_info.Code s) :: (Odoc_info.Raw " ") :: t
        | Odoc_info.See_doc s -> (Odoc_info.Italic [Odoc_info.Raw s]) :: (Odoc_info.Raw " ") :: t
      in
      self#html_of_text b t_ref

    (** Print html code for the given list of "see also" references.*)
    method html_of_sees b l =
      match l with
        [] -> ()
      | see :: [] ->
          bp b "<b>%s</b> " Odoc_messages.see_also;
          self#html_of_see b see;
          bs b "<br>\n"
      | _ ->
          bp b "<b>%s</b><ul>" Odoc_messages.see_also;
          List.iter
            (fun see ->
              bs b "<li>" ;
              self#html_of_see b see;
              bs b "</li>\n"
            )
            l;
          bs b "</ul>\n"

    (** Print html code for the given optional return information.*)
    method html_of_return_opt b return_opt =
      match return_opt with
        None -> ()
      | Some s ->
          bp b "<b>%s</b> " Odoc_messages.returns;
          self#html_of_text b s;
          bs b "<br>\n"

    (** Print html code for the given list of custom tagged texts. *)
    method html_of_custom b l =
      List.iter
        (fun (tag, text) ->
          try
            let f = List.assoc tag tag_functions in
            Buffer.add_string b (f text)
          with
            Not_found ->
              Odoc_info.warning (Odoc_messages.tag_not_handled tag)
        )
        l

    (** Print html code for a description, except for the [i_params] field.
       @param indent can be specified not to use the style of info comments;
       default is [true].
    *)
    method html_of_info ?(cls="") ?(indent=true) b info_opt =
      match info_opt with
        None ->
          ()
      | Some info ->
          let module M = Odoc_info in
          if indent then bs b ("<div class=\"info "^cls^"\">\n");
          (
           match info.M.i_deprecated with
            None -> ()
           | Some d ->
               bs b "<span class=\"warning\">";
               bs b Odoc_messages.deprecated ;
               bs b "</span>" ;
               self#html_of_text b d;
               bs b "<br>\n"
          );
          (
           match info.M.i_desc with
             None -> ()
           | Some d when d = [Odoc_info.Raw ""] -> ()
           | Some d -> self#html_of_text b d; bs b "<br>\n"
          );
          self#html_of_author_list b info.M.i_authors;
          self#html_of_version_opt b info.M.i_version;
          self#html_of_before b info.M.i_before;
          self#html_of_since_opt b info.M.i_since;
          self#html_of_raised_exceptions b info.M.i_raised_exceptions;
          self#html_of_return_opt b info.M.i_return_value;
          self#html_of_sees b info.M.i_sees;
          self#html_of_custom b info.M.i_custom;
          if indent then bs b "</div>\n"

    (** Print html code for the first sentence of a description.
       The titles and lists in this first sentence has been removed.*)
    method html_of_info_first_sentence b info_opt =
      match info_opt with
        None -> ()
      | Some info ->
          let module M = Odoc_info in
          let dep = info.M.i_deprecated <> None in
          bs b "<div class=\"info\">\n";
          if dep then bs b "<span class=\"deprecated\">";
          (
           match info.M.i_desc with
             None -> ()
           | Some d when d = [Odoc_info.Raw ""] -> ()
           | Some d ->
               self#html_of_text b
                 (Odoc_info.text_no_title_no_list
                    (Odoc_info.first_sentence_of_text d));
               bs b "\n"
          );
          if dep then bs b "</span>";
          bs b "</div>\n"

  end



let opt = Odoc_info.apply_opt

let print_concat b sep f =
  let rec iter = function
      [] -> ()
    | [c] -> f c
    | c :: q ->
        f c;
        bs b sep;
        iter q
  in
  iter

let newline_to_indented_br s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '\n' -> Buffer.add_string b "<br>     "
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

module Generator =
  struct
(** This class is used to create objects which can generate a simple html documentation. *)
class html =
  object (self)
    inherit text
    inherit info

    val mutable doctype =
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n"
    method character_encoding () =
      Printf.sprintf
        "<meta content=\"text/html; charset=%s\" http-equiv=\"Content-Type\">\n"
        !charset

    (** The default style options. *)
    val mutable default_style_options =
      [ ".keyword { font-weight : bold ; color : Red }" ;
        ".keywordsign { color : #C04600 }" ;
        ".comment { color : Green }" ;
        ".constructor { color : Blue }" ;
        ".type { color : #5C6585 }" ;
        ".string { color : Maroon }" ;
        ".warning { color : Red ; font-weight : bold }" ;
        ".info { margin-left : 3em; margin-right: 3em }" ;
        ".param_info { margin-top: 4px; margin-left : 3em; margin-right : 3em }" ;
        ".code { color : #465F91 ; }" ;
        ".typetable { border-style : hidden }" ;
        ".paramstable { border-style : hidden ; padding: 5pt 5pt}" ;
        "tr { background-color : White }" ;
        "td.typefieldcomment { background-color : #FFFFFF ; font-size: smaller ;}" ;
        "div.sig_block {margin-left: 2em}" ;
        "*:target { background: yellow; }" ;

        "body {font: 13px sans-serif; color: black; text-align: left; padding: 5px; margin: 0}";

        "h1 { font-size : 20pt ; text-align: center; }" ;

        "h2 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #90BDFF ;"^
        "padding: 2px; }" ;

        "h3 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #90DDFF ;"^
        "padding: 2px; }" ;

        "h4 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #90EDFF ;"^
        "padding: 2px; }" ;

        "h5 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #90FDFF ;"^
        "padding: 2px; }" ;

        "h6 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #90BDFF ; "^
        "padding: 2px; }" ;

        "div.h7 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #E0FFFF ; "^
        "padding: 2px; }" ;

        "div.h8 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #F0FFFF ; "^
        "padding: 2px; }" ;

        "div.h9 { font-size : 20pt ; border: 1px solid #000000; "^
        "margin-top: 5px; margin-bottom: 2px;"^
        "text-align: center; background-color: #FFFFFF ; "^
        "padding: 2px; }" ;

        "a {color: #416DFF; text-decoration: none}";
        "a:hover {background-color: #ddd; text-decoration: underline}";
        "pre { margin-bottom: 4px; font-family: monospace; }" ;
        "pre.verbatim, pre.codepre { }";

        ".indextable {border: 1px #ddd solid; border-collapse: collapse}";
        ".indextable td, .indextable th {border: 1px #ddd solid; min-width: 80px}";
        ".indextable td.module {background-color: #eee ;  padding-left: 2px; padding-right: 2px}";
        ".indextable td.module a {color: 4E6272; text-decoration: none; display: block; width: 100%}";
        ".indextable td.module a:hover {text-decoration: underline; background-color: transparent}";
        ".deprecated {color: #888; font-style: italic}" ;

        ".indextable tr td div.info { margin-left: 2px; margin-right: 2px }" ;

        "ul.indexlist { margin-left: 0; padding-left: 0;}";
        "ul.indexlist li { list-style-type: none ; margin-left: 0; padding-left: 0; }";
      ]

    (** The style file for all pages. *)
    val mutable style_file = "style.css"

    (** The code to import the style. Initialized in [init_style]. *)
    val mutable style = ""

    (** The known types names.
       Used to know if we must create a link to a type
       when printing a type. *)
    val mutable known_types_names = StringSet.empty

    (** The known class and class type names.
       Used to know if we must create a link to a class
       or class type or not when printing a type. *)
    val mutable known_classes_names = StringSet.empty

    (** The known modules and module types names.
       Used to know if we must create a link to a type or not
       when printing a module type. *)
    val mutable known_modules_names = StringSet.empty

    method index_prefix =
      if !Odoc_global.out_file = Odoc_messages.default_out_file then
        "index"
      else
        Filename.basename !Odoc_global.out_file

    (** The main file. *)
    method index =
      let p = self#index_prefix in
      Printf.sprintf "%s.html" p

    (** The file for the index of values. *)
    method index_values = Printf.sprintf "%s_values.html" self#index_prefix

    (** The file for the index of types. *)
    method index_types = Printf.sprintf "%s_types.html" self#index_prefix

    (** The file for the index of extensions. *)
    method index_extensions = Printf.sprintf "%s_extensions.html" self#index_prefix

    (** The file for the index of exceptions. *)
    method index_exceptions = Printf.sprintf "%s_exceptions.html" self#index_prefix

    (** The file for the index of attributes. *)
    method index_attributes = Printf.sprintf "%s_attributes.html" self#index_prefix

    (** The file for the index of methods. *)
    method index_methods = Printf.sprintf "%s_methods.html" self#index_prefix

    (** The file for the index of classes. *)
    method index_classes = Printf.sprintf "%s_classes.html" self#index_prefix

    (** The file for the index of class types. *)
    method index_class_types = Printf.sprintf "%s_class_types.html" self#index_prefix

    (** The file for the index of modules. *)
    method index_modules = Printf.sprintf "%s_modules.html" self#index_prefix

    (** The file for the index of module types. *)
    method index_module_types = Printf.sprintf "%s_module_types.html" self#index_prefix


    (** The list of attributes. Filled in the [generate] method. *)
    val mutable list_attributes = []
    method list_attributes = list_attributes

    (** The list of methods. Filled in the [generate] method. *)
    val mutable list_methods = []
    method list_methods = list_methods

    (** The list of values. Filled in the [generate] method. *)
    val mutable list_values = []
    method list_values = list_values

    (** The list of extensions. Filled in the [generate] method. *)
    val mutable list_extensions = []
    method list_extensions = list_extensions

    (** The list of exceptions. Filled in the [generate] method. *)
    val mutable list_exceptions = []
    method list_exceptions = list_exceptions

    (** The list of types. Filled in the [generate] method. *)
    val mutable list_types = []
    method list_types = list_types

    (** The list of modules. Filled in the [generate] method. *)
    val mutable list_modules = []
    method list_modules = list_modules

    (** The list of module types. Filled in the [generate] method. *)
    val mutable list_module_types = []
    method list_module_types = list_module_types

    (** The list of classes. Filled in the [generate] method. *)
    val mutable list_classes = []
    method list_classes = list_classes

    (** The list of class types. Filled in the [generate] method. *)
    val mutable list_class_types = []
    method list_class_types = list_class_types

    (** The header of pages. Must be prepared by the [prepare_header] method.*)
    val mutable header = fun _ -> fun ?nav:_ -> fun ?comments:_ -> fun _ -> ()

    (** Init the style. *)
    method init_style =
      (match !css_style with
        None ->
          let default_style = String.concat "\n" default_style_options in
          (
           try
             let file = Filename.concat !Global.target_dir style_file in
             if Sys.file_exists file then
               Odoc_info.verbose (Odoc_messages.file_exists_dont_generate file)
             else
               (
                let chanout = open_out file in
                output_string chanout default_style ;
                flush chanout ;
                close_out chanout;
                Odoc_info.verbose (Odoc_messages.file_generated file)
               )
           with
             Sys_error s ->
               prerr_endline s ;
               incr Odoc_info.errors ;
          )
      | Some f ->
          style_file <- f
      );
      style <- "<link rel=\"stylesheet\" href=\""^style_file^"\" type=\"text/css\">\n"

    (** Get the title given by the user *)
    method title = match !Global.title with None -> "" | Some t -> self#escape t

    (** Get the title given by the user completed with the given subtitle. *)
    method inner_title s =
      (match self#title with "" -> "" | t -> t^" : ")^
      (self#escape s)

    (** Get the page header. *)
    method print_header b ?nav ?comments title = header b ?nav ?comments title

    (** A function to build the header of pages. *)
    method prepare_header module_list =
      let f b ?(nav=None) ?(comments=[]) t  =
        let link_if_not_empty l m url =
          match l with
            [] -> ()
          | _ ->
              bp b "<link title=\"%s\" rel=Appendix href=\"%s\">\n" m url
        in
        bs b "<head>\n";
        bs b style;
        bs b (self#character_encoding ()) ;
        bs b "<link rel=\"Start\" href=\"";
        bs b self#index;
        bs b "\">\n" ;
        (
         match nav with
           None -> ()
         | Some (pre_opt, post_opt, name) ->
             (match pre_opt with
               None -> ()
             | Some name ->
                 bp b "<link rel=\"previous\" href=\"%s\">\n"
                   (fst (Naming.html_files name));
             );
             (match post_opt with
               None -> ()
             | Some name ->
                 bp b "<link rel=\"next\" href=\"%s\">\n"
                   (fst (Naming.html_files name));
             );
             (
              let father = Name.father name in
              let href = if father = "" then self#index else fst (Naming.html_files father) in
              bp b "<link rel=\"Up\" href=\"%s\">\n" href
             )
        );
        link_if_not_empty self#list_types Odoc_messages.index_of_types self#index_types;
        link_if_not_empty self#list_extensions Odoc_messages.index_of_extensions self#index_extensions;
        link_if_not_empty self#list_exceptions Odoc_messages.index_of_exceptions self#index_exceptions;
        link_if_not_empty self#list_values Odoc_messages.index_of_values self#index_values;
        link_if_not_empty self#list_attributes Odoc_messages.index_of_attributes self#index_attributes;
        link_if_not_empty self#list_methods Odoc_messages.index_of_methods self#index_methods;
        link_if_not_empty self#list_classes Odoc_messages.index_of_classes self#index_classes;
        link_if_not_empty self#list_class_types Odoc_messages.index_of_class_types self#index_class_types;
        link_if_not_empty self#list_modules Odoc_messages.index_of_modules self#index_modules;
        link_if_not_empty self#list_module_types Odoc_messages.index_of_module_types self#index_module_types;
        let print_one m =
          let html_file = fst (Naming.html_files m.m_name) in
          bp b "<link title=\"%s\" rel=\"Chapter\" href=\"%s\">"
            m.m_name html_file
        in
        print_concat b "\n" print_one module_list;
        self#html_sections_links b comments;
        bs b "<title>";
        bs b t ;
        bs b "</title>\n</head>\n"
      in
      header <- f

    (** Build the html code for the link tags in the header, defining section and
       subsections for the titles found in the given comments.*)
    method html_sections_links b comments =
      let titles = List.flatten (List.map Odoc_info.get_titles_in_text comments) in
      let levels =
        let rec iter acc l =
          match l with
            [] -> acc
          | (n,_,_) :: q ->
              if List.mem n acc
              then iter acc q
              else iter (n::acc) q
        in
        iter [] titles
      in
      let sorted_levels = List.sort compare levels in
      let (section_level, subsection_level) =
        match sorted_levels with
          [] -> (None, None)
        | [n] -> (Some n, None)
        | n :: m :: _ -> (Some n, Some m)
      in
      let titles_per_level level_opt =
        match level_opt with
          None -> []
        | Some n -> List.filter (fun (m,_,_) -> m = n) titles
      in
      let section_titles = titles_per_level section_level in
      let subsection_titles = titles_per_level subsection_level in
      let print_lines s_rel titles =
        List.iter
          (fun (n,lopt,t) ->
            let s = Odoc_info.string_of_text t in
            let label = self#create_title_label (n,lopt,t) in
            bp b "<link title=\"%s\" rel=\"%s\" href=\"#%s\">\n" s s_rel label
          )
          titles
      in
      print_lines "Section" section_titles ;
      print_lines "Subsection" subsection_titles


    (** Html code for navigation bar.
       @param pre optional name for optional previous module/class
       @param post optional name for optional next module/class
       @param name name of current module/class *)
    method print_navbar b pre post name =
      bs b "<div class=\"navbar\">";
      (
       match pre with
         None -> ()
       | Some name ->
           bp b "<a class=\"pre\" href=\"%s\" title=\"%s\">%s</a>\n"
             (fst (Naming.html_files name))
             name
             Odoc_messages.previous
      );
      bs b "&nbsp;";
      let father = Name.father name in
      let href = if father = "" then self#index else fst (Naming.html_files father) in
      let father_name = if father = "" then "Index" else father in
      bp b "<a class=\"up\" href=\"%s\" title=\"%s\">%s</a>\n" href father_name Odoc_messages.up;
      bs b "&nbsp;";
      (
       match post with
         None -> ()
       | Some name ->
           bp b "<a class=\"post\" href=\"%s\" title=\"%s\">%s</a>\n"
             (fst (Naming.html_files name))
             name
             Odoc_messages.next
      );
      bs b "</div>\n"

    (** Return html code with the given string in the keyword style.*)
    method keyword s =
      "<span class=\"keyword\">"^s^"</span>"

    (** Return html code with the given string in the constructor style. *)
    method constructor s = "<span class=\"constructor\">"^s^"</span>"

    (** Output the given ocaml code to the given file name. *)
    method private output_code in_title file code =
      try
        let chanout = open_out file in
        let b = new_buf () in
        bs b "<html>";
        self#print_header b (self#inner_title in_title);
        bs b"<body>\n";
        self#html_of_code b code;
        bs b "</body></html>";
        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          incr Odoc_info.errors ;
          prerr_endline s

    (** Take a string and return the string where fully qualified
       type (or class or class type) idents
       have been replaced by links to the type referenced by the ident.*)
    method create_fully_qualified_idents_links m_name s =
      let f str_t =
        let match_s = Str.matched_string str_t in
        let rel = Name.get_relative m_name match_s in
        let s_final = Odoc_info.apply_if_equal
            Odoc_info.use_hidden_modules
            match_s
            rel
        in
        if StringSet.mem match_s known_types_names then
           "<a href=\""^(Naming.complete_target Naming.mark_type match_s)^"\">"^
           s_final^
           "</a>"
        else
          if StringSet.mem match_s known_classes_names then
            let (html_file, _) = Naming.html_files match_s in
            "<a href=\""^html_file^"\">"^s_final^"</a>"
          else
            s_final
      in
      Str.global_substitute
        (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)")
        f
        s

    (** Take a string and return the string where fully qualified module idents
       have been replaced by links to the module referenced by the ident.*)
    method create_fully_qualified_module_idents_links m_name s =
      let f str_t =
        let match_s = Str.matched_string str_t in
        let rel = Name.get_relative m_name match_s in
        let s_final = Odoc_info.apply_if_equal
            Odoc_info.use_hidden_modules
            match_s
            rel
        in
        if StringSet.mem match_s known_modules_names then
          let (html_file, _) = Naming.html_files match_s in
          "<a href=\""^html_file^"\">"^s_final^"</a>"
        else
          s_final
      in
      Str.global_substitute
        (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\)\\(\\.[A-Z][a-zA-Z_'0-9]*\\)*")
        f
        s

    (** Print html code to display a [Types.type_expr]. *)
    method html_of_type_expr b m_name t =
      let s = Odoc_info.remove_ending_newline (Odoc_info.string_of_type_expr t) in
      let s2 = newline_to_indented_br s in
      bs b "<code class=\"type\">";
      bs b (self#create_fully_qualified_idents_links m_name s2);
      bs b "</code>"

    (** Print html code to display a [Types.type_expr list]. *)
    method html_of_cstr_args ?par b m_name sep l =
      print_DEBUG "html#html_of_cstr_args";
      let s =
        match l with
        | Cstr_tuple l ->
            Odoc_info.string_of_type_list ?par sep l
        | Cstr_record l ->
            Odoc_info.string_of_record l
      in
      print_DEBUG "html#html_of_cstr_args: 1";
      let s2 = newline_to_indented_br s in
      print_DEBUG "html#html_of_cstr_args: 2";
      bs b "<code class=\"type\">";
      bs b (self#create_fully_qualified_idents_links m_name s2);
      bs b "</code>"

    (** Print html code to display a [Types.type_expr list] as type parameters
       of a class of class type. *)
    method html_of_class_type_param_expr_list b m_name l =
      let s = Odoc_info.string_of_class_type_param_list l in
      let s2 = newline_to_indented_br s in
      bs b "<code class=\"type\">[";
      bs b (self#create_fully_qualified_idents_links m_name s2);
      bs b "]</code>"

    method html_of_class_parameter_list b father c =
      let s = Odoc_info.string_of_class_params c in
      let s = Odoc_info.remove_ending_newline s in
      let s2 = newline_to_indented_br s in
      bs b "<code class=\"type\">";
      bs b (self#create_fully_qualified_idents_links father s2);
      bs b "</code>"

    (** Print html code to display a list of type parameters for the given type.*)
    method html_of_type_expr_param_list b m_name t =
      let s = Odoc_info.string_of_type_param_list t in
      let s2 = newline_to_indented_br s in
      bs b "<code class=\"type\">";
      bs b (self#create_fully_qualified_idents_links m_name s2);
      bs b "</code>"

    (** Print html code to display a [Types.module_type]. *)
    method html_of_module_type b ?code m_name t =
      let s = Odoc_info.remove_ending_newline (Odoc_info.string_of_module_type ?code t) in
      bs b "<code class=\"type\">";
      bs b (self#create_fully_qualified_module_idents_links m_name s);
      bs b "</code>"

    (** Print html code to display the given module kind. *)
    method html_of_module_kind b father ?modu kind =
      match kind with
        Module_struct eles ->
          self#html_of_text b [Code "sig"];
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
          self#html_of_text b [Code "end"]
      | Module_alias a ->
          bs b "<code class=\"type\">";
          bs b (self#create_fully_qualified_module_idents_links father a.ma_name);
          bs b "</code>"
      | Module_functor (p, k) ->
          if !html_short_functors then
            bs b " "
          else
            bs b "<div class=\"sig_block\">";
          self#html_of_module_parameter b father p;
          (
           match k with
             Module_functor _ -> ()
           | _ when !html_short_functors ->
               bs b ": "
           | _ -> ()
          );
          self#html_of_module_kind b father ?modu k;
          if not !html_short_functors then
            bs b "</div>"
      | Module_apply (k1, k2) ->
          (* TODO: application is not correct in a .mli.
             What to do -> print typedtree module_type    *)
          self#html_of_module_kind b father k1;
          self#html_of_text b [Code "("];
          self#html_of_module_kind b father k2;
          self#html_of_text b [Code ")"]
      | Module_with (k, s) ->
          (* TODO: modify when Module_with will be more detailed *)
          self#html_of_module_type_kind b father ?modu k;
          bs b "<code class=\"type\"> ";
          bs b (self#create_fully_qualified_module_idents_links father s);
          bs b "</code>"
      | Module_constraint (k, _tk) ->
          (* TODO: what to print ? *)
          self#html_of_module_kind b father ?modu k
      | Module_typeof s ->
          bs b "<code class=\"type\">module type of ";
          bs b (self#create_fully_qualified_module_idents_links father s);
          bs b "</code>"
      | Module_unpack (code, mta) ->
          bs b "<code class=\"type\">";
          begin
            match mta.mta_module with
              None ->
                bs b (self#create_fully_qualified_module_idents_links father (self#escape code))
            | Some mt ->
                let (html_file, _) = Naming.html_files mt.mt_name in
                bp b " <a href=\"%s\">%s</a> " html_file (self#escape code)
          end;
          bs b "</code>"


    method html_of_module_parameter b father p =
      let (s_functor,s_arrow) =
        if !html_short_functors then
          "", ""
        else
          "functor ", "-> "
      in
      self#html_of_text b
        [
          Code (s_functor^"(");
          Code p.mp_name ;
          Code " : ";
        ] ;
      self#html_of_module_type_kind b father p.mp_kind;
      self#html_of_text b [ Code (") "^s_arrow)]

    method html_of_module_element b m_name ele =
      match ele with
        Element_module m ->
          self#html_of_module b ~complete: false m
      | Element_module_type mt ->
          self#html_of_modtype b ~complete: false mt
      | Element_included_module im ->
          self#html_of_included_module b im
      | Element_class c ->
          self#html_of_class b ~complete: false c
      | Element_class_type ct ->
          self#html_of_class_type b ~complete: false ct
      | Element_value v ->
          self#html_of_value b v
      | Element_type_extension te ->
          self#html_of_type_extension b m_name te
      | Element_exception e ->
          self#html_of_exception b e
      | Element_type t ->
          self#html_of_type b t
      | Element_module_comment text ->
          self#html_of_module_comment b text

    (** Print html code to display the given module type kind. *)
    method html_of_module_type_kind b father ?modu ?mt kind =
      match kind with
        Module_type_struct eles ->
          self#html_of_text b [Code "sig"];
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
          self#html_of_text b [Code "end"]
      | Module_type_functor (p, k) ->
          self#html_of_module_parameter b father p;
          self#html_of_module_type_kind b father ?modu ?mt k
      | Module_type_alias a ->
          bs b "<code class=\"type\">";
          bs b (self#create_fully_qualified_module_idents_links father a.mta_name);
          bs b "</code>"
      | Module_type_with (k, s) ->
          self#html_of_module_type_kind b father ?modu ?mt k;
          bs b "<code class=\"type\"> ";
          bs b (self#create_fully_qualified_module_idents_links father s);
          bs b "</code>"
      | Module_type_typeof s ->
          bs b "<code class=\"type\">module type of ";
          bs b (self#create_fully_qualified_module_idents_links father s);
          bs b "</code>"

    (** Print html code to display the type of a module parameter.. *)
    method html_of_module_parameter_type b m_name p =
      match p.mp_type with None -> bs b "<code>()</code>"
      | Some mty -> self#html_of_module_type b m_name ~code: p.mp_type_code mty

    (** Generate a file containing the module type in the given file name. *)
    method output_module_type in_title file mtyp =
      let s = Odoc_info.remove_ending_newline (Odoc_info.string_of_module_type ~complete: true mtyp) in
      self#output_code in_title file s

    (** Generate a file containing the class type in the given file name. *)
    method output_class_type in_title file ctyp =
      let s = Odoc_info.remove_ending_newline (Odoc_info.string_of_class_type ~complete: true ctyp) in
      self#output_code in_title file s

    (** Print html code for a value. *)
    method html_of_value b v =
      Odoc_info.reset_type_names ();
      bs b "\n<pre>" ;
      bp b "<span id=\"%s\">" (Naming.value_target v);
      bs b (self#keyword "val");
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
       if !with_parameter_list then
         self#html_of_parameter_list b (Name.father v.val_name) v.val_parameters
       else
         self#html_of_described_parameter_list b (Name.father v.val_name) v.val_parameters
      )

    (** Print html code for a type extension. *)
    method html_of_type_extension b m_name te =
      Odoc_info.reset_type_names ();
      bs b "<pre><code>";
      bs b ((self#keyword "type")^" ");
      let s = Odoc_info.string_of_type_extension_param_list te in
      let s2 = newline_to_indented_br s in
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
              Cstr_tuple [], None -> ()
            | l,None ->
                bs b (" " ^ (self#keyword "of") ^ " ");
                self#html_of_cstr_args ~par: false b father " * " l;
            | Cstr_tuple [],Some r ->
                bs b (" " ^ (self#keyword ":") ^ " ");
                self#html_of_type_expr b father r;
            | l,Some r ->
                bs b (" " ^ (self#keyword ":") ^ " ");
                self#html_of_cstr_args ~par: false b father " * " l;
                bs b (" " ^ (self#keyword "->") ^ " ");
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
        print_concat b "\n" print_one te.te_constructors;
        bs b "</table>\n";
        bs b "\n";
        self#html_of_info b te.te_info;
        bs b "\n"

    (** Print html code for an exception. *)
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
          Cstr_tuple [], None -> ()
        | _,None ->
            bs b (" "^(self#keyword "of")^" ");
            self#html_of_cstr_args
                   ~par: false b (Name.father e.ex_name) " * " e.ex_args
        | Cstr_tuple [],Some r ->
            bs b (" " ^ (self#keyword ":") ^ " ");
            self#html_of_type_expr b (Name.father e.ex_name) r;
        | l,Some r ->
            bs b (" " ^ (self#keyword ":") ^ " ");
            self#html_of_cstr_args
                   ~par: false b (Name.father e.ex_name) " * " l;
            bs b (" " ^ (self#keyword "->") ^ " ");
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

    (** Print html code for a type. *)
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
      self#html_of_type_expr_param_list b father t;
      (match t.ty_parameters with [] -> () | _ -> bs b " ");
      bs b (Name.simple t.ty_name);
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
           print_concat b "\n" print_one fields;
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
               Cstr_tuple [], None -> ()
             | l,None ->
                 bs b (" " ^ (self#keyword "of") ^ " ");
                 self#html_of_cstr_args ~par: false b father " * " l;
             | Cstr_tuple [],Some r ->
                 bs b (" " ^ (self#keyword ":") ^ " ");
                 self#html_of_type_expr b father r;
             | l,Some r ->
                 bs b (" " ^ (self#keyword ":") ^ " ");
                 self#html_of_cstr_args ~par: false b father " * " l;
                 bs b (" " ^ (self#keyword "->") ^ " ");
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
          print_concat b "\n" print_one l;
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
            bs b ";</code></td>\n";
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
          print_concat b "\n" print_one l;
          bs b "</table>\n}\n"
      | Type_open ->
          bs b "= ..";
          bs b "</pre>"
      );
      bs b "\n";
      self#html_of_info b t.ty_info;
      bs b "\n"

    (** Print html code for a class attribute. *)
    method html_of_attribute b a =
      let module_name = Name.father (Name.father a.att_value.val_name) in
      bs b "\n<pre>" ;
      bp b "<span id=\"%s\">" (Naming.attribute_target a);
      bs b (self#keyword "val");
      bs b " ";
      (
       if a.att_virtual then
         bs b ((self#keyword "virtual")^ " ")
       else
         ()
      );
      (
       if a.att_mutable then
         bs b ((self#keyword Odoc_messages.mutab)^ " ")
       else
         ()
      );(
       match a.att_value.val_code with
         None -> bs b (Name.simple a.att_value.val_name)
       | Some c ->
           let file = Naming.file_code_attribute_complete_target a in
           self#output_code a.att_value.val_name (Filename.concat !Global.target_dir file) c;
           bp b "<a href=\"%s\">%s</a>" file (Name.simple a.att_value.val_name);
      );
      bs b "</span>";
      bs b " : ";
      self#html_of_type_expr b module_name a.att_value.val_type;
      bs b "</pre>";
      self#html_of_info b a.att_value.val_info

    (** Print html code for a class method. *)
    method html_of_method b m =
      let module_name = Name.father (Name.father m.met_value.val_name) in
      bs b "\n<pre>";
      (* html mark *)
      bp b "<span id=\"%s\">" (Naming.method_target m);
     bs b ((self#keyword "method")^" ");
       if m.met_private then bs b ((self#keyword "private")^" ");
      if m.met_virtual then bs b ((self#keyword "virtual")^" ");
      (
       match m.met_value.val_code with
         None -> bs b  (Name.simple m.met_value.val_name)
       | Some c ->
           let file = Naming.file_code_method_complete_target m in
           self#output_code m.met_value.val_name (Filename.concat !Global.target_dir file) c;
           bp b "<a href=\"%s\">%s</a>" file (Name.simple m.met_value.val_name);
      );
      bs b "</span>";
      bs b " : ";
      self#html_of_type_expr b module_name m.met_value.val_type;
      bs b "</pre>";
      self#html_of_info b m.met_value.val_info;
      (
       if !with_parameter_list then
         self#html_of_parameter_list b
           module_name m.met_value.val_parameters
       else
         self#html_of_described_parameter_list b
           module_name m.met_value.val_parameters
      )

    (** Print html code for the description of a function parameter. *)
    method html_of_parameter_description b p =
      match Parameter.names p with
        [] ->
          ()
      | name :: [] ->
          (
           (* Only one name, no need for label for the description. *)
           match Parameter.desc_by_name p name with
             None -> ()
           | Some t -> self#html_of_text b t
          )
      | l ->
          (*  A list of names, we display those with a description. *)
          let l2 = List.filter
              (fun n -> (Parameter.desc_by_name p n) <> None)
              l
          in
          let print_one n =
            match Parameter.desc_by_name p n with
              None -> ()
            | Some t ->
                bs b "<code>";
                bs b n;
                bs b "</code> : ";
                self#html_of_text b t
          in
          print_concat b "<br>\n" print_one l2

    (** Print html code for a list of parameters. *)
    method html_of_parameter_list b m_name l =
      match l with
        [] -> ()
      | _ ->
          bs b "<div class=\"param_info\">";
          bs b "<table border=\"0\" cellpadding=\"3\" width=\"100%\">\n";
          bs b "<tr>\n<td align=\"left\" valign=\"top\" width=\"1%\">";
          bs b "<b>";
          bs b Odoc_messages.parameters;
          bs b ": </b></td>\n" ;
          bs b "<td>\n<table class=\"paramstable\">\n";
          let print_one p =
            bs b "<tr>\n<td align=\"center\" valign=\"top\" width=\"15%\" class=\"code\">\n";
            bs b
              (
               match Parameter.complete_name p with
                 "" -> "?"
               | s -> s
              );
            bs b "</td>\n<td align=\"center\" valign=\"top\">:</td>\n";
            bs b "<td>";
            self#html_of_type_expr b m_name (Parameter.typ p);
            bs b "<br>\n";
            self#html_of_parameter_description b p;
            bs b "\n</tr>\n";
          in
          List.iter print_one l;
          bs b "</table>\n</td>\n</tr>\n</table></div>\n"

    (** Print html code for the parameters which have a name and description. *)
    method html_of_described_parameter_list b _m_name l =
      (* get the params which have a name, and at least one name described. *)
      let l2 = List.filter
          (fun p ->
            List.exists
              (fun n -> (Parameter.desc_by_name p n) <> None)
              (Parameter.names p))
          l
      in
      let f p =
        bs b "<div class=\"param_info\"><code class=\"code\">";
        bs b (Parameter.complete_name p);
        bs b "</code> : " ;
        self#html_of_parameter_description b p;
        bs b "</div>\n"
      in
      List.iter f l2

    (** Print html code for a list of module parameters. *)
    method html_of_module_parameter_list b m_name l =
      match l with
        [] ->
          ()
      | _ ->
          bs b "<table border=\"0\" cellpadding=\"3\" width=\"100%\">\n";
          bs b "<tr>\n";
          bs b "<td align=\"left\" valign=\"top\" width=\"1%%\"><b>";
          bs b Odoc_messages.parameters ;
          bs b ": </b></td>\n<td>\n";
          bs b "<table class=\"paramstable\">\n";
          List.iter
            (fun (p, desc_opt) ->
              bs b "<tr>\n";
              bs b "<td align=\"center\" valign=\"top\" width=\"15%\">\n<code>" ;
              bs b p.mp_name;
              bs b "</code></td>\n" ;
              bs b "<td align=\"center\" valign=\"top\">:</td>\n";
              bs b "<td>" ;
              self#html_of_module_parameter_type b m_name p;
              bs b "\n";
              (
               match desc_opt with
                 None -> ()
               | Some t ->
                   bs b "<br>";
                   self#html_of_text b t;
                   bs b "\n</tr>\n" ;
              )
            )
            l;
          bs b "</table>\n</td>\n</tr>\n</table>\n"

    (** Print html code for a module. *)
    method html_of_module b ?(info=true) ?(complete=true) ?(with_link=true) m =
      let (html_file, _) = Naming.html_files m.m_name in
      let father = Name.father m.m_name in
      bs b "\n<pre>";
      bs b ((self#keyword "module")^" ");
      (
       if with_link then
         bp b "<a href=\"%s\">%s</a>" html_file (Name.simple m.m_name)
       else
         bs b (Name.simple m.m_name)
      );
      (
       match m.m_kind with
         Module_functor _ when !html_short_functors  ->
           ()
       | _ -> bs b ": "
      );
      self#html_of_module_kind b father ~modu: m m.m_kind;
      bs b "</pre>";
      if info then
        (
         if complete then
           self#html_of_info ~cls: "module top" ~indent: true
         else
           self#html_of_info_first_sentence
        ) b m.m_info
      else
        ()

    (** Print html code for a module type. *)
    method html_of_modtype b ?(info=true) ?(complete=true) ?(with_link=true) mt =
      let (html_file, _) = Naming.html_files mt.mt_name in
      let father = Name.father mt.mt_name in
      bs b "\n<pre>";
      bs b ((self#keyword "module type")^" ");
      (
       if with_link then
         bp b "<a href=\"%s\">%s</a>" html_file (Name.simple mt.mt_name)
         else
         bs b (Name.simple mt.mt_name)
      );
      (match mt.mt_kind with
        None -> ()
      | Some k ->
          bs b " = ";
          self#html_of_module_type_kind b father ~mt k
      );
      bs b "</pre>";
      if info then
        (
         if complete then
           self#html_of_info ~cls: "modtype top" ~indent: true
         else
           self#html_of_info_first_sentence
        ) b mt.mt_info
      else
        ()

    (** Print html code for an included module. *)
    method html_of_included_module b im =
      bs b "\n<pre>";
      bs b ((self#keyword "include")^" ");
      (
       match im.im_module with
         None ->
           bs b im.im_name
       | Some mmt ->
           let (file, name) =
             match mmt with
               Mod m ->
                 let (html_file, _) = Naming.html_files m.m_name in
                 (html_file, m.m_name)
             | Modtype mt ->
                 let (html_file, _) = Naming.html_files mt.mt_name in
                 (html_file, mt.mt_name)
           in
           bp b "<a href=\"%s\">%s</a>" file name
      );
      bs b "</pre>\n";
      self#html_of_info b im.im_info

    method html_of_class_element b element =
      match element with
        Class_attribute a ->
          self#html_of_attribute b a
      | Class_method m ->
          self#html_of_method b m
      | Class_comment t ->
          self#html_of_class_comment b t

    method html_of_class_kind b father ?cl kind =
      match kind with
        Class_structure (inh, eles) ->
          self#html_of_text b [Code "object"];
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
          self#html_of_text b [Code "end"]

      | Class_apply _ ->
          (* TODO: display final type from typedtree *)
          self#html_of_text b [Raw "class application not handled yet"]

      | Class_constr cco ->
          (
           match cco.cco_type_parameters with
             [] -> ()
           | l ->
               self#html_of_class_type_param_expr_list b father l;
               bs b " "
          );
          bs b "<code class=\"type\">";
          bs b (self#create_fully_qualified_idents_links father cco.cco_name);
          bs b "</code>"

      | Class_constraint (ck, ctk) ->
          self#html_of_text b [Code "( "] ;
          self#html_of_class_kind b father ck;
          self#html_of_text b [Code " : "] ;
          self#html_of_class_type_kind b father ctk;
          self#html_of_text b [Code " )"]

    method html_of_class_type_kind b father ?ct kind =
      match kind with
        Class_type cta ->
          (
           match cta.cta_type_parameters with
             [] -> ()
           | l ->
               self#html_of_class_type_param_expr_list b father l;
               bs b " "
          );
          bs b "<code class=\"type\">";
          bs b (self#create_fully_qualified_idents_links father cta.cta_name);
          bs b "</code>"

      | Class_signature (inh, eles) ->
          self#html_of_text b [Code "object"];
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
          self#html_of_text b [Code "end"]

    (** Print html code for a class. *)
    method html_of_class b ?(complete=true) ?(with_link=true) c =
      let father = Name.father c.cl_name in
      Odoc_info.reset_type_names ();
      let (html_file, _) = Naming.html_files c.cl_name in
      bs b "\n<pre>";
      (* we add a html id, the same as for a type so we can
         go directly here when the class name is used as a type name *)
      bp b "<span name=\"%s\">"
        (Naming.type_target
           { ty_name = c.cl_name ;
             ty_info = None ; ty_parameters = [] ;
             ty_kind = Type_abstract ; ty_private = Asttypes.Public; ty_manifest = None ;
             ty_loc = Odoc_info.dummy_loc ;
             ty_code = None ;
           }
        );
      bs b ((self#keyword "class")^" ");
      print_DEBUG "html#html_of_class : virtual or not" ;
      if c.cl_virtual then bs b ((self#keyword "virtual")^" ");
      (
       match c.cl_type_parameters with
         [] -> ()
       | l ->
           self#html_of_class_type_param_expr_list b father l;
           bs b " "
      );
      print_DEBUG "html#html_of_class : with link or not" ;
      (
       if with_link then
         bp b "<a href=\"%s\">%s</a>" html_file (Name.simple c.cl_name)
       else
         bs b (Name.simple c.cl_name)
      );
      bs b "</span>";
      bs b " : " ;
      self#html_of_class_parameter_list b father c ;
      self#html_of_class_kind b father ~cl: c c.cl_kind;
      bs b "</pre>" ;
      print_DEBUG "html#html_of_class : info" ;
      (
       if complete then
         self#html_of_info ~cls: "class top" ~indent: true
       else
         self#html_of_info_first_sentence
      ) b c.cl_info

    (** Print html code for a class type. *)
    method html_of_class_type b ?(complete=true) ?(with_link=true) ct =
      Odoc_info.reset_type_names ();
      let father = Name.father ct.clt_name in
      let (html_file, _) = Naming.html_files ct.clt_name in
      bs b "\n<pre>";
      (* we add a html id, the same as for a type so we can
         go directly here when the class type name is used as a type name *)
      bp b "<span id=\"%s\">"
        (Naming.type_target
           { ty_name = ct.clt_name ;
             ty_info = None ; ty_parameters = [] ;
             ty_kind = Type_abstract ; ty_private = Asttypes.Public; ty_manifest = None ;
             ty_loc = Odoc_info.dummy_loc ;
             ty_code = None ;
           }
        );
      bs b ((self#keyword "class type")^" ");
      if ct.clt_virtual then bs b ((self#keyword "virtual")^" ");
      (
       match ct.clt_type_parameters with
        [] -> ()
      | l ->
          self#html_of_class_type_param_expr_list b father l;
          bs b " "
      );

      if with_link then
        bp b "<a href=\"%s\">%s</a>" html_file (Name.simple ct.clt_name)
      else
        bs b (Name.simple ct.clt_name);

      bs b "</span>";
      bs b " = ";
      self#html_of_class_type_kind b father ~ct ct.clt_kind;
      bs b "</pre>";
      (
       if complete then
         self#html_of_info ~cls: "classtype top" ~indent: true
       else
         self#html_of_info_first_sentence
      ) b ct.clt_info

    (** Return html code to represent a dag, represented as in Odoc_dag2html. *)
    method html_of_dag dag =
      let f n =
        let (name, cct_opt) = n.Odoc_dag2html.valu in
        (* if we have a c_opt = Some class then we take its information
           because we are sure the name is complete. *)
        let (name2, html_file) =
          match cct_opt with
            None -> (name, fst (Naming.html_files name))
          | Some (Cl c) -> (c.cl_name, fst (Naming.html_files c.cl_name))
          | Some (Cltype (ct, _)) -> (ct.clt_name, fst (Naming.html_files ct.clt_name))
        in
        let new_v =
          "<table border=1>\n<tr><td>"^
          "<a href=\""^html_file^"\">"^name2^"</a>"^
          "</td></tr>\n</table>\n"
        in
        { n with Odoc_dag2html.valu = new_v }
      in
      let a = Array.map f dag.Odoc_dag2html.dag in
      Odoc_dag2html.html_of_dag { Odoc_dag2html.dag = a }

    (** Print html code for a module comment.*)
    method html_of_module_comment b text =
      bs b "<br>\n";
      self#html_of_text b text;
      bs b "<br>\n"

    (** Print html code for a class comment.*)
    method html_of_class_comment b text =
      (* Add some style if there is no style for the first part of the text. *)
      let text2 =
        match text with
        | (Odoc_info.Raw s) :: q ->
            (Odoc_info.Title (2, None, [Odoc_info.Raw s])) :: q
        | _ -> text
      in
      self#html_of_text b text2

    (** Generate html code for the given list of inherited classes.*)
    method generate_inheritance_info b inher_l =
      let f inh =
        match inh.ic_class with
          None -> (* we can't make the link. *)
            (Odoc_info.Code inh.ic_name) ::
            (match inh.ic_text with
              None -> []
            | Some t -> (Odoc_info.Raw "    ") :: t)
        | Some cct ->
            (* we can create the link. *)
            let real_name = (* even if it should be the same *)
              match cct with
                Cl c -> c.cl_name
              | Cltype (ct, _) -> ct.clt_name
            in
            let (class_file, _) = Naming.html_files real_name in
            (Odoc_info.Link (class_file, [Odoc_info.Code real_name])) ::
            (match inh.ic_text with
              None -> []
            | Some t -> (Odoc_info.Raw "    ") :: t)
      in
      let text = [
        Odoc_info.Bold [Odoc_info.Raw Odoc_messages.inherits] ;
        Odoc_info.List (List.map f inher_l)
      ]
      in
      self#html_of_text b text

    (** Generate html code for the inherited classes of the given class. *)
    method generate_class_inheritance_info b cl =
      let rec iter_kind k =
        match k with
          Class_structure ([], _) ->
            ()
        | Class_structure (l, _) ->
            self#generate_inheritance_info b l
        | Class_constraint (k, _) ->
            iter_kind k
        | Class_apply _
        | Class_constr _ ->
            ()
      in
      iter_kind cl.cl_kind

    (** Generate html code for the inherited classes of the given class type. *)
    method generate_class_type_inheritance_info b clt =
      match clt.clt_kind with
        Class_signature ([], _) ->
          ()
      | Class_signature (l, _) ->
          self#generate_inheritance_info b l
      | Class_type _ ->
          ()

    (** A method to create index files. *)
    method generate_elements_index :
        'a.
        'a list ->
          ('a -> Odoc_info.Name.t) ->
            ('a -> Odoc_info.info option) ->
              ('a -> string) -> string -> string -> unit =
    fun elements name info target title simple_file ->
      try
        let chanout = open_out (Filename.concat !Global.target_dir simple_file) in
        let b = new_buf () in
        bs b "<html>\n";
        self#print_header b (self#inner_title title);
        bs b "<body>\n";
        self#print_navbar b None None "";
        bs b "<h1>";
        bs b title;
        bs b "</h1>\n" ;

        let sorted_elements = List.sort
            (fun e1 e2 -> compare (Name.simple (name e1)) (Name.simple (name e2)))
            elements
        in
        let groups = Odoc_info.create_index_lists sorted_elements (fun e -> Name.simple (name e)) in
        let f_ele e =
          let simple_name = Name.simple (name e) in
          let father_name = Name.father (name e) in
          bp b "<tr><td><a href=\"%s\">%s</a> " (target e) (self#escape simple_name);
          if simple_name <> father_name && father_name <> "" then
            bp b "[<a href=\"%s\">%s</a>]" (fst (Naming.html_files father_name)) father_name;
          bs b "</td>\n<td>";
          self#html_of_info_first_sentence b (info e);
          bs b "</td></tr>\n";
        in
        let f_group l =
          match l with
            [] -> ()
          | e :: _ ->
              let s =
                match (Char.uppercase_ascii (Name.simple (name e)).[0]) with
                  'A'..'Z' as c -> String.make 1 c
                | _ -> ""
              in
              bs b "<tr><td align=\"left\"><br>";
              bs b s ;
              bs b "</td></tr>\n" ;
              List.iter f_ele l
        in
        bs b "<table>\n";
        List.iter f_group groups ;
        bs b "</table>\n" ;
        bs b "</body>\n</html>";
        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          raise (Failure s)

    (** A method to generate a list of module/class files. *)
    method generate_elements :
        'a. ('a option -> 'a option -> 'a -> unit) -> 'a list -> unit =
      fun f_generate l ->
        let rec iter pre_opt = function
            [] -> ()
          | ele :: [] -> f_generate pre_opt None ele
          | ele1 :: ele2 :: q ->
              f_generate pre_opt (Some ele2) ele1 ;
              iter (Some ele1) (ele2 :: q)
        in
        iter None l

    (** Generate the code of the html page for the given class.*)
    method generate_for_class pre post cl =
      Odoc_info.reset_type_names ();
      let (html_file, _) = Naming.html_files cl.cl_name in
      let type_file = Naming.file_type_class_complete_target cl.cl_name in
      try
        let chanout = open_out (Filename.concat !Global.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun c -> c.cl_name) pre in
        let post_name = opt (fun c -> c.cl_name) post in
        bs b doctype ;
        bs b "<html>\n";
        self#print_header b
          ~nav: (Some (pre_name, post_name, cl.cl_name))
          ~comments: (Class.class_comments cl)
          (self#inner_title cl.cl_name);
        bs b "<body>\n";
        self#print_navbar b pre_name post_name cl.cl_name;
        bs b "<h1>";
        bs b (Odoc_messages.clas^" ");
        if cl.cl_virtual then bs b "virtual " ;
        bp b "<a href=\"%s\">%s</a>" type_file cl.cl_name;
        bs b "</h1>\n";
        self#html_of_class b ~with_link: false cl;
        (* parameters *)
        self#html_of_described_parameter_list b
          (Name.father cl.cl_name) cl.cl_parameters;
        (* class inheritance *)
        self#generate_class_inheritance_info b cl;
        (* a horizontal line *)
        bs b "<hr width=\"100%\">\n";
        (* the various elements *)
        List.iter (self#html_of_class_element b)
          (Class.class_elements ~trans:false cl);
        bs b "</body></html>";
        Buffer.output_buffer chanout b;
        close_out chanout;

        (* generate the file with the complete class type *)
        self#output_class_type
          cl.cl_name
          (Filename.concat !Global.target_dir type_file)
          cl.cl_type
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the code of the html page for the given class type.*)
    method generate_for_class_type pre post clt =
      Odoc_info.reset_type_names ();
      let (html_file, _) = Naming.html_files clt.clt_name in
      let type_file = Naming.file_type_class_complete_target clt.clt_name in
      try
        let chanout = open_out (Filename.concat !Global.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun ct -> ct.clt_name) pre in
        let post_name = opt (fun ct -> ct.clt_name) post in
        bs b doctype ;
        bs b "<html>\n";
        self#print_header b
          ~nav: (Some (pre_name, post_name, clt.clt_name))
          ~comments: (Class.class_type_comments clt)
          (self#inner_title clt.clt_name);

        bs b "<body>\n";
        self#print_navbar b pre_name post_name clt.clt_name;
        bs b "<h1>";
        bs b (Odoc_messages.class_type^" ");
        if clt.clt_virtual then bs b "virtual ";
        bp b "<a href=\"%s\">%s</a>" type_file clt.clt_name;
        bs b "</h1>\n";
        self#html_of_class_type b ~with_link: false clt;

        (* class inheritance *)
        self#generate_class_type_inheritance_info b clt;
        (* a horizontal line *)
        bs b "<hr width=\"100%\">\n";
        (* the various elements *)
        List.iter (self#html_of_class_element b)
          (Class.class_type_elements ~trans: false clt);
        bs b "</body></html>";
        Buffer.output_buffer chanout b;
        close_out chanout;

        (* generate the file with the complete class type *)
        self#output_class_type
          clt.clt_name
          (Filename.concat !Global.target_dir type_file)
          clt.clt_type
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the html file for the given module type.
       @raise Failure if an error occurs.*)
    method generate_for_module_type pre post mt =
      try
        let (html_file, _) = Naming.html_files mt.mt_name in
        let type_file = Naming.file_type_module_complete_target mt.mt_name in
        let chanout = open_out (Filename.concat !Global.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun mt -> mt.mt_name) pre in
        let post_name = opt (fun mt -> mt.mt_name) post in
        bs b doctype ;
        bs b "<html>\n";
        self#print_header b
          ~nav: (Some (pre_name, post_name, mt.mt_name))
          ~comments: (Module.module_type_comments mt)
          (self#inner_title mt.mt_name);
        bs b "<body>\n";
        self#print_navbar b pre_name post_name mt.mt_name;
        bp b "<h1>";
        bs b (Odoc_messages.module_type^" ");
        (
         match mt.mt_type with
           Some _ -> bp b "<a href=\"%s\">%s</a>" type_file mt.mt_name
         | None-> bs b mt.mt_name
        );
        bs b "</h1>\n" ;
        self#html_of_modtype b ~with_link: false mt;

        (* parameters for functors *)
        self#html_of_module_parameter_list b
          (Name.father mt.mt_name)
          (Module.module_type_parameters mt);
        (* a horizontal line *)
        bs b "<hr width=\"100%\">\n";
        (* module elements *)
        List.iter
          (self#html_of_module_element b mt.mt_name)
          (Module.module_type_elements mt);

        bs b "</body></html>";
        Buffer.output_buffer chanout b;
        close_out chanout;

        (* generate html files for submodules *)
        self#generate_elements self#generate_for_module (Module.module_type_modules mt);
        (* generate html files for module types *)
        self#generate_elements self#generate_for_module_type (Module.module_type_module_types mt);
        (* generate html files for classes *)
        self#generate_elements self#generate_for_class (Module.module_type_classes mt);
        (* generate html files for class types *)
        self#generate_elements self#generate_for_class_type (Module.module_type_class_types mt);

        (* generate the file with the complete module type *)
        (
         match mt.mt_type with
           None -> ()
         | Some mty ->
             self#output_module_type
               mt.mt_name
               (Filename.concat !Global.target_dir type_file)
               mty
        )
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the html file for the given module.
       @raise Failure if an error occurs.*)
    method generate_for_module pre post modu =
      try
        Odoc_info.verbose ("Generate for module "^modu.m_name);
        let (html_file, _) = Naming.html_files modu.m_name in
        let type_file = Naming.file_type_module_complete_target modu.m_name in
        let code_file = Naming.file_code_module_complete_target modu.m_name in
        let chanout = open_out (Filename.concat !Global.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun m -> m.m_name) pre in
        let post_name = opt (fun m -> m.m_name) post in
        bs b doctype ;
        bs b "<html>\n";
        self#print_header b
          ~nav: (Some (pre_name, post_name, modu.m_name))
          ~comments: (Module.module_comments modu)
          (self#inner_title modu.m_name);
        bs b "<body>\n" ;
        self#print_navbar b pre_name post_name modu.m_name ;
        bs b "<h1>";
        if modu.m_text_only then
          bs b modu.m_name
        else
          (
           bs b
             (
              if Module.module_is_functor modu then
                Odoc_messages.functo
              else
                Odoc_messages.modul
             );
           bp b " <a href=\"%s\">%s</a>" type_file modu.m_name;
           (
            match modu.m_code with
              None -> ()
            | Some _ -> bp b " (<a href=\"%s\">.ml</a>)" code_file
           )
          );
        bs b "</h1>\n";

        if not modu.m_text_only then self#html_of_module b ~with_link: false modu;

        (* parameters for functors *)
        self#html_of_module_parameter_list b
          (Name.father modu.m_name)
          (Module.module_parameters modu);

        (* a horizontal line *)
        if not modu.m_text_only then bs b "<hr width=\"100%\">\n";

        (* module elements *)
        List.iter
          (self#html_of_module_element b modu.m_name)
          (Module.module_elements modu);

        bs b "</body></html>";
        Buffer.output_buffer chanout b;
        close_out chanout;

        (* generate html files for submodules *)
        self#generate_elements  self#generate_for_module (Module.module_modules modu);
        (* generate html files for module types *)
        self#generate_elements  self#generate_for_module_type (Module.module_module_types modu);
        (* generate html files for classes *)
        self#generate_elements  self#generate_for_class (Module.module_classes modu);
        (* generate html files for class types *)
        self#generate_elements  self#generate_for_class_type (Module.module_class_types modu);

        (* generate the file with the complete module type *)
        self#output_module_type
          modu.m_name
          (Filename.concat !Global.target_dir type_file)
          modu.m_type;

        match modu.m_code with
          None -> ()
        | Some code ->
            self#output_code
              modu.m_name
              (Filename.concat !Global.target_dir code_file)
              code
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the [<index_prefix>.html] file corresponding to the given module list.
       @raise Failure if an error occurs.*)
    method generate_index module_list =
      try
        let chanout = open_out (Filename.concat !Global.target_dir self#index) in
        let b = new_buf () in
        let title = match !Global.title with None -> "" | Some t -> self#escape t in
        bs b doctype ;
        bs b "<html>\n";
        self#print_header b self#title;
        bs b "<body>\n";

        bs b "<h1>";
        bs b title;
        bs b "</h1>\n" ;
        let info = Odoc_info.apply_opt
            (Odoc_info.info_of_comment_file module_list)
            !Odoc_info.Global.intro_file
        in
        (
         match info with
           None ->
             self#html_of_Index_list b;
             bs b "<br/>";
             self#html_of_Module_list b
               (List.map (fun m -> m.m_name) module_list);
         | Some _ -> self#html_of_info ~indent: false b info
        );
        bs b "</body>\n</html>";
        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the values index in the file [index_values.html]. *)
    method generate_values_index _module_list =
      self#generate_elements_index
        self#list_values
        (fun v -> v.val_name)
        (fun v -> v.val_info)
        Naming.complete_value_target
        Odoc_messages.index_of_values
        self#index_values

    (** Generate the extensions index in the file [index_extensions.html]. *)
    method generate_extensions_index _module_list =
      self#generate_elements_index
        self#list_extensions
        (fun x -> x.xt_name)
        (fun x -> x.xt_type_extension.te_info)
        (fun x -> Naming.complete_extension_target x)
        Odoc_messages.index_of_extensions
        self#index_extensions

    (** Generate the exceptions index in the file [index_exceptions.html]. *)
    method generate_exceptions_index _module_list =
      self#generate_elements_index
        self#list_exceptions
        (fun e -> e.ex_name)
        (fun e -> e.ex_info)
        Naming.complete_exception_target
        Odoc_messages.index_of_exceptions
        self#index_exceptions

    (** Generate the types index in the file [index_types.html]. *)
    method generate_types_index _module_list =
      self#generate_elements_index
        self#list_types
        (fun t -> t.ty_name)
        (fun t -> t.ty_info)
        Naming.complete_type_target
        Odoc_messages.index_of_types
        self#index_types

    (** Generate the attributes index in the file [index_attributes.html]. *)
    method generate_attributes_index _module_list =
      self#generate_elements_index
        self#list_attributes
        (fun a -> a.att_value.val_name)
        (fun a -> a.att_value.val_info)
        Naming.complete_attribute_target
        Odoc_messages.index_of_attributes
        self#index_attributes

    (** Generate the methods index in the file [index_methods.html]. *)
    method generate_methods_index _module_list =
      self#generate_elements_index
        self#list_methods
        (fun m -> m.met_value.val_name)
        (fun m -> m.met_value.val_info)
        Naming.complete_method_target
        Odoc_messages.index_of_methods
        self#index_methods

    (** Generate the classes index in the file [index_classes.html]. *)
    method generate_classes_index _module_list =
      self#generate_elements_index
        self#list_classes
        (fun c -> c.cl_name)
        (fun c -> c.cl_info)
        (fun c -> fst (Naming.html_files c.cl_name))
        Odoc_messages.index_of_classes
        self#index_classes

    (** Generate the class types index in the file [index_class_types.html]. *)
    method generate_class_types_index _module_list =
      self#generate_elements_index
        self#list_class_types
        (fun ct -> ct.clt_name)
        (fun ct -> ct.clt_info)
        (fun ct -> fst (Naming.html_files ct.clt_name))
        Odoc_messages.index_of_class_types
        self#index_class_types

    (** Generate the modules index in the file [index_modules.html]. *)
    method generate_modules_index _module_list =
      self#generate_elements_index
        self#list_modules
        (fun m -> m.m_name)
        (fun m -> m.m_info)
        (fun m -> fst (Naming.html_files m.m_name))
        Odoc_messages.index_of_modules
        self#index_modules

    (** Generate the module types index in the file [index_module_types.html]. *)
    method generate_module_types_index _module_list =
      self#generate_elements_index
        self#list_module_types
        (fun mt -> mt.mt_name)
        (fun mt -> mt.mt_info)
        (fun mt -> fst (Naming.html_files mt.mt_name))
        Odoc_messages.index_of_module_types
        self#index_module_types

    (** Generate all the html files from a module list. The main
       file is [<index_prefix>.html]. *)
    method generate module_list =
      (* init the style *)
      self#init_style ;
      (* init the lists of elements *)
      list_values <- Odoc_info.Search.values module_list ;
      list_extensions <- Odoc_info.Search.extensions module_list ;
      list_exceptions <- Odoc_info.Search.exceptions module_list ;
      list_types <- Odoc_info.Search.types module_list ;
      list_attributes <- Odoc_info.Search.attributes module_list ;
      list_methods <- Odoc_info.Search.methods module_list ;
      list_classes <- Odoc_info.Search.classes module_list ;
      list_class_types <- Odoc_info.Search.class_types module_list ;
      list_modules <- Odoc_info.Search.modules module_list ;
      list_module_types <- Odoc_info.Search.module_types module_list ;

      (* prepare the page header *)
      self#prepare_header module_list ;
      (* Get the names of all known types. *)
      let types = Odoc_info.Search.types module_list in
      known_types_names <-
        List.fold_left
          (fun acc t -> StringSet.add t.ty_name acc)
          known_types_names
          types ;
      (* Get the names of all class and class types. *)
      let classes = Odoc_info.Search.classes module_list in
      let class_types = Odoc_info.Search.class_types module_list in
      known_classes_names <-
        List.fold_left
          (fun acc c -> StringSet.add c.cl_name acc)
          known_classes_names
          classes ;
      known_classes_names <-
        List.fold_left
          (fun acc ct -> StringSet.add ct.clt_name acc)
          known_classes_names
          class_types ;
      (* Get the names of all known modules and module types. *)
      let module_types = Odoc_info.Search.module_types module_list in
      let modules = Odoc_info.Search.modules module_list in
      known_modules_names <-
        List.fold_left
          (fun acc m -> StringSet.add m.m_name acc)
          known_modules_names
          modules ;
      known_modules_names <-
        List.fold_left
          (fun acc mt -> StringSet.add mt.mt_name acc)
          known_modules_names
          module_types ;
      (* generate html for each module *)
      if not !index_only then
        self#generate_elements self#generate_for_module module_list ;

      try
        self#generate_index module_list;
        self#generate_values_index module_list ;
        self#generate_extensions_index module_list ;
        self#generate_exceptions_index module_list ;
        self#generate_types_index module_list ;
        self#generate_attributes_index module_list ;
        self#generate_methods_index module_list ;
        self#generate_classes_index module_list ;
        self#generate_class_types_index module_list ;
        self#generate_modules_index module_list ;
        self#generate_module_types_index module_list ;
      with
        Failure s ->
          prerr_endline s ;
          incr Odoc_info.errors

    initializer
      Odoc_ocamlhtml.html_of_comment :=
        (fun s ->
          let b = new_buf () in
          self#html_of_text b (Odoc_text.Texter.text_of_string s);
          Buffer.contents b
        )
  end
end

module type Html_generator = module type of Generator

*)
