(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

(* This is the Reason lexer. As stated in src/README, there's a good section in
  Real World OCaml that describes what a lexer is:

  https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html

  Basically, it uses regular expressions to first cut the big code string into
  more meaningful chunks, called tokens. For example, it cuts the string

    let foo = 1

  into `let`, `foo`, `=` and `1`, massage them a bit into nice variants, then
  send the data structures into the parser `reason_parser.mly`, which takes
  care of the next step (turning the stream of tokens into an AST, abstract
  syntax tree).

  The file's syntax's a bit special. It's not the conventional OCaml syntax. An
  ordinary language syntax isn't expressive/convenient enough for a lexer. This
  mll ("ml lexer") syntax is a fine-tuned variation of the usual OCaml syntax.
 *)

{
open Lexing
open Reason_parser
open Reason_errors
open Lexer_warning

(* The table of keywords *)

let keyword_table, reverse_keyword_table =
  let create_hashtable n l =
    let t = Hashtbl.create n in
    let rev_t = Hashtbl.create n in
    List.iter (fun (k, v) ->
      Hashtbl.add t k v;
      Hashtbl.add rev_t v k;
    ) l;
    t, rev_t
  in
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "esfun", ES6_FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "switch", SWITCH;
    "module", MODULE;
    "pub", PUB;
    "mutable", MUTABLE;
    "new", NEW;
    "nonrec", NONREC;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
(*  "parser", PARSER; *)
    "pri", PRI;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lor", INFIXOP3("lor");
    "lxor", INFIXOP3("lxor");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
]

(* Specialize raise_error for lexing errors *)

let raise_error error loc = raise_error (Lexing_error error) loc

(* Internal exception to escape from core lexer loop when a string is not
 * terminated *)

exception Unterminated of Location.t

(* Reify internal state of the lexer *)

type state = {
  string_buffer: Buffer.t;
  raw_buffer: Buffer.t;
  mutable string_start_loc: Location.t;
  mutable comment_start_loc: Location.t list;
}

type string_context = {
  string_start_loc: Location.t;
  string_delim: string;
  string_in_comment: bool;
  string_buffer: Buffer.t;
  string_raw_buffer: Buffer.t;
}

let make () = {
  string_buffer = Buffer.create 256;
  raw_buffer = Buffer.create 256;
  (* To store the position of the beginning of a string and comment *)
  string_start_loc = Location.none;
  comment_start_loc = [];
}

let store_string_char buffer c =
  Buffer.add_char buffer c

let store_string state s =
  Buffer.add_string state.string_buffer s

let store_lexeme buffer lexbuf =
  Buffer.add_string buffer (Lexing.lexeme lexbuf)

let get_stored_string state =
  let str = Buffer.contents state.string_buffer in
  let raw =
    if Buffer.length state.raw_buffer = 0
    then None
    else Some (Buffer.contents state.raw_buffer)
  in
  Buffer.reset state.string_buffer;
  Buffer.reset state.raw_buffer;
  (str, raw)

let list_first_last = function
  | [] -> invalid_arg "list_first_last: argument cannot be the empty list"
  | (first :: _) as list ->
    let rec aux = function
      | [] -> assert false
      | [last] -> last
      | _ :: xs -> aux xs
    in
    (first, aux list)

(* To "unlex" a few characters *)
let set_lexeme_length buf n = (
  let open Lexing in
  if n < 0 then
    invalid_arg "set_lexeme_length: offset should be positive";
  if n > buf.lex_curr_pos - buf.lex_start_pos then
    invalid_arg "set_lexeme_length: offset larger than lexeme";
  buf.lex_curr_pos <- buf.lex_start_pos + n;
  buf.lex_curr_p <- {buf.lex_start_p
                     with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
)

(* This cut comment characters of the current buffer.
 * Operators (including "/*" and "//") are lexed with the same rule, and this
 * function cuts the lexeme at the beginning of an operator. *)
let lexeme_without_comment buf = (
  let lexeme = Lexing.lexeme buf in
  let i = ref 0 and len = String.length lexeme - 1 in
  let found = ref (-1) in
  while !i < len && !found = -1 do
    begin match lexeme.[!i], lexeme.[!i+1] with
      | ('/', '*') | ('/', '/') | ('*', '/') ->
        found := !i;
      | _ -> ()
    end;
    incr i
  done;
  match !found with
  | -1 -> lexeme
  | n ->
      set_lexeme_length buf n;
      String.sub lexeme 0 n
)

(* Operators that could conflict with comments (those containing /*, */ and //)
 * are escaped in the source. The lexer removes the escapes so that the
 * identifier looks like OCaml ones.
 * An escape in first position is kept to distinguish "verbatim" operators
 * (\=== for instance). *)
let unescape_operator str =
  if (str <> "" && String.contains_from str 1 '\\') then (
    let b = Buffer.create (String.length str) in
    Buffer.add_char b str.[0];
    for i = 1 to String.length str - 1 do
      let c = str.[i] in
      if c <> '\\' then Buffer.add_char b c
    done;
    Buffer.contents b
  ) else str

let lexeme_operator lexbuf =
  unescape_operator (lexeme_without_comment lexbuf)

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then (
    raise_error
        (Illegal_escape (Lexing.lexeme lexbuf))
        (Location.curr lexbuf);
    'x'
  ) else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* To convert integer literals, allowing max_int + 1 (PR#4210) *)

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0
                                                       (String.length s - 1)))

(* Remove underscores from float literals *)

let remove_underscores s =
  let l = String.length s in
  let b = Bytes.create l in
  let rec remove src dst =
    if src >= l then
      if dst >= l then s else Bytes.sub_string b 0 dst
    else
      match s.[src] with
        '_' -> remove (src + 1) dst
      |  c  -> Bytes.set b dst c; remove (src + 1) (dst + 1)
  in remove 0 0

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

}


let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let uppercase_or_lowercase = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let lowercase_latin1 = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase_latin1 = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar_latin1 =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let operator_chars =
  ['!' '$' '%' '&' '+' '-' ':' '<' '=' '>' '?' '@' '^' '|' '~' '#' '.'] |
  ( '\\'? ['/' '*'] )

let decimal_literal = ['0'-'9'] ['0'-'9' '_']*

let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*

let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal

let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?

let literal_modifier = ['G'-'Z' 'g'-'z']

rule token state = parse
  | "\\" newline {
      raise_error
        (Illegal_character (Lexing.lexeme_char lexbuf 0))
        (Location.curr lexbuf);
      update_loc lexbuf None 1 false 0;
      token state lexbuf
    }
  | newline
    { update_loc lexbuf None 1 false 0;
      token state lexbuf
    }
  | blank +
    { token state lexbuf }
  | "_"
    { UNDERSCORE }
  | "~"
    { TILDE }
  | "?"
    { QUESTION }
  | "=?"
    { set_lexeme_length lexbuf 1; EQUAL }
  | lowercase identchar *
    { let s = Lexing.lexeme lexbuf in
      try Hashtbl.find keyword_table s
      with Not_found -> LIDENT s
    }
  | lowercase_latin1 identchar_latin1 *
    { warn_latin1 lexbuf; LIDENT (Lexing.lexeme lexbuf) }
  | uppercase identchar *
    { UIDENT(Lexing.lexeme lexbuf) }       (* No capitalized keywords *)
  | uppercase_latin1 identchar_latin1 *
    { warn_latin1 lexbuf; UIDENT(Lexing.lexeme lexbuf) }
  | int_literal
    { INT (Lexing.lexeme lexbuf, None) }
  | (int_literal as lit) (literal_modifier as modif)
    { INT (lit, Some modif) }
  | float_literal | hex_float_literal
    { FLOAT (Lexing.lexeme lexbuf, None) }
  | ((float_literal | hex_float_literal) as lit) (literal_modifier as modif)
    { FLOAT (lit, Some modif) }
  | ((float_literal | hex_float_literal) as lit) identchar+
    { raise_error
        (Invalid_literal (Lexing.lexeme lexbuf))
        (Location.curr lexbuf);
      FLOAT (lit, None)
    }
  | (int_literal as lit) identchar+
    { raise_error
        (Invalid_literal (Lexing.lexeme lexbuf))
        (Location.curr lexbuf);
      INT (lit, None)
    }
  | "\""
    { let string_start = lexbuf.lex_start_p in
      state.string_start_loc <- Location.curr lexbuf;
      string false state.raw_buffer lexbuf;
      lexbuf.lex_start_p <- string_start;
      let text, raw = get_stored_string state in
      STRING (text, raw, None)
    }
  | "{" (lowercase* as delim) "|"
    { let string_start = lexbuf.lex_start_p in
      state.string_start_loc <- Location.curr lexbuf;
      quoted_string delim lexbuf;
      lexbuf.lex_start_p <- string_start;
      STRING (fst (get_stored_string state), None, Some delim)
    }
  | "'" (newline as c) "'"
    { update_loc lexbuf None 1 false 1;
      CHAR c
    }
  | "'" ([^ '\\' '\'' '\010' '\013'] as c) "'"
    { CHAR c }
  | "'\\" (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c) "'"
    { CHAR (char_for_backslash c) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { CHAR (char_for_decimal_code lexbuf 2) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { CHAR (char_for_hexadecimal_code lexbuf 3) }
  | "'" (("\\" _) as esc)
    { raise_error (Illegal_escape esc) (Location.curr lexbuf);
      token state lexbuf
    }
  | "#=<"
    { (* Allow parsing of foo#=<bar /> *)
      set_lexeme_length lexbuf 2;
      SHARPEQUAL
    }
  | "#="
    { SHARPEQUAL }
  | "#" operator_chars+
    { SHARPOP(lexeme_operator lexbuf) }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
    { update_loc lexbuf name (int_of_string num) true 0;
      token state lexbuf
    }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "`"  { BACKQUOTE }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "*"  { STAR }
  | ","  { COMMA }
  | "->" { MINUSGREATER }
  | "=>" { EQUALGREATER }
  (* allow lexing of | `Variant =><Component /> *)
  | "=><" uppercase_or_lowercase (identchar | '.') * {
    set_lexeme_length lexbuf 2;
    EQUALGREATER
  }
  | "#"  { SHARP }
  | "."  { DOT }
  | ".." { DOTDOT }
  | "..."{ DOTDOTDOT }
  | ":"  { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | ":>" { COLONGREATER }
  | ";"  { SEMI }
  | ";;" { SEMISEMI }
  | "<"  { LESS }
  | "="  { EQUAL }
  | "["  { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "[<" { LBRACKETLESS }
  | "[>" { LBRACKETGREATER }
  | "<" (uppercase identchar* '.')* lowercase identchar* {
    let buf = Lexing.lexeme lexbuf in
    LESSIDENT (String.sub buf 1 (String.length buf - 1))
  }
  | ">..." { GREATERDOTDOTDOT }
  (* Allow parsing of Pexp_override:
   * let z = {<state: 0, x: y>};
   *
   * Make sure {<state is emitted as LBRACELESS.
   * This contrasts with jsx:
   * in a jsx context {<div needs to be LBRACE LESS (two tokens)
   * for a valid parse.
   *)
  | "{<" uppercase_or_lowercase identchar* blank*  ":" {
    set_lexeme_length lexbuf 2;
    LBRACELESS
  }
  | "{<" uppercase_or_lowercase (identchar | '.') * {
    (* allows parsing of `{<Text` in <Description term={<Text text="Age" />}> as correct jsx *)
    set_lexeme_length lexbuf 1;
    LBRACE
  }
  | "</" blank* uppercase_or_lowercase (identchar | '.') * blank* ">" {
    let buf = Lexing.lexeme lexbuf in
    LESSSLASHIDENTGREATER (String.trim (String.sub buf 2 (String.length buf - 2 - 1)))
  }
  | "]"  { RBRACKET }
  | "{"  { LBRACE }
  | "{<" { LBRACELESS }
  | "|"  { BAR }
  | "||" { BARBAR }
  | "|]" { BARRBRACKET }
  | ">"  { GREATER }
  (* Having a GREATERRBRACKET makes it difficult to parse patterns such
     as > ]. The space in between then becomes significant and must be
     maintained when printing etc. >] isn't even needed!
  | ">]" { GREATERRBRACKET }
  *)
  | "}"  { RBRACE }
  | ">}" { GREATERRBRACE }
  | "=<" uppercase_or_lowercase+ {
    (* allow `let x=<div />;` *)
    set_lexeme_length lexbuf 1;
    EQUAL
  }
  (* jsx in arrays: [|<div />|]*)
  | "/>|]" {
    set_lexeme_length lexbuf 2;
    SLASHGREATER
  }
  | "[|<" {
    set_lexeme_length lexbuf 2;
    LBRACKETBAR
  }
    (* allow parsing of <div /></Component> *)
  | "/></" uppercase_or_lowercase+ {
    (* allow parsing of <div asd=1></div> *)
    set_lexeme_length lexbuf 2;
    SLASHGREATER
  }
  | "></" uppercase_or_lowercase+ {
    (* allow parsing of <div asd=1></div> *)
    set_lexeme_length lexbuf 1;
    GREATER
  }
  | "><" uppercase_or_lowercase+ {
    (* allow parsing of <div><span> *)
    set_lexeme_length lexbuf 1;
    GREATER
  }
  | "[@" { LBRACKETAT }
  | "[%" { LBRACKETPERCENT }
  | "[%%" { LBRACKETPERCENTPERCENT }
  | "!"  { BANG }
  | "!=" { INFIXOP0 "!=" }
  | "!==" { INFIXOP0 "!==" }
  | "\\!=" { INFIXOP0 "!=" }
  | "\\!==" { INFIXOP0 "!==" }
  | "+"  { PLUS }
  | "+." { PLUSDOT }
  | "+=" { PLUSEQ }
  | "-"  { MINUS }
  | "-." { MINUSDOT }
  | "<>" { LESSGREATER }
  | "</>" { LESSSLASHGREATER }
  | "<..>" { LESSDOTDOTGREATER }
  | '\\'? ['~' '?' '!'] operator_chars+
            { PREFIXOP(lexeme_operator lexbuf) }
  | '\\'? ['=' '<' '>' '|' '&' '$'] operator_chars*
            {
              INFIXOP0(lexeme_operator lexbuf)
            }
  | '\\'? '@' operator_chars*
            { INFIXOP1(lexeme_operator lexbuf) }
  | '\\'? '^' ('\\' '.')? operator_chars*
            { match lexeme_without_comment lexbuf with
              | "^." -> set_lexeme_length lexbuf 1; POSTFIXOP("^")
              | "^|" ->
                  (* ^| is not an infix op in [|a^|] *)
                  set_lexeme_length lexbuf 1; POSTFIXOP("^")
              | "^" -> POSTFIXOP("^")
              | op -> INFIXOP1(unescape_operator op) }
  | "++" operator_chars*
            { INFIXOP1(lexeme_operator lexbuf) }
  | '\\'? ['+' '-'] operator_chars*
            { INFIXOP2(lexeme_operator lexbuf) }
  (* SLASHGREATER is an INFIXOP3 that is handled specially *)
  | "/>" { SLASHGREATER }
  (* The second star must be escaped so that the precedence assumptions for
   * printing match those of parsing. (Imagine what could happen if the other
   * rule beginning with * picked up */*, and we internally escaped it to **.
   * Whe printing, we have an understanding of the precedence of "**", which
   * enables us to safely print/group it, but that understanding would not
   * match the *actual* precedence that it was parsed at thanks to the *other*
   * rule beginning with *, picking it up instead of the special double ** rule
   * below.
   *)
  | '\\'? '*' '\\'? '*' operator_chars*
            { INFIXOP4(lexeme_operator lexbuf) }
  | '%'     { PERCENT }
  | '\\'? ['/' '*'] operator_chars*
            { match lexeme_operator lexbuf with
              | "" ->
                  (* If the operator is empty, it means the lexeme is beginning
                   * by a comment sequence: we let the comment lexer handle
                   * the case. *)
                  enter_comment lexbuf
              | op -> INFIXOP3 op }
  | '%' operator_chars*
            { INFIXOP3(lexeme_operator lexbuf) }
  | eof { EOF }
  | _
    { raise_error
        (Illegal_character (Lexing.lexeme_char lexbuf 0))
        (Location.curr lexbuf);
      token state lexbuf
    }

and enter_comment buffer = parse
  | "//" ([^'\010']* newline as line)
    { update_loc lexbuf None 1 false 0;
      let physical_loc = Location.curr lexbuf in
      let location = { physical_loc with
        loc_end = { physical_loc.loc_end with
          (* Don't track trailing `\n` in the location
           * 1| // comment
           * 2| let x = 1;
           * By omitting the `\n` at the end of line 1, the location of the
           * comment spans line 1. Otherwise the comment on line 1 would end
           * on the second line. The printer looks at the closing pos_lnum
           * location to interleave whitespace correct. It needs to align
           * with what we visually see (i.e. it ends on line 1) *)
          pos_lnum = physical_loc.loc_end.pos_lnum - 1;
          pos_cnum = physical_loc.loc_end.pos_cnum + 1;
      }} in
      COMMENT (line, location)
    }
  | "//" ([^'\010']* eof as line)
    { update_loc lexbuf None 1 false 0;
      let physical_loc = Location.curr lexbuf in
      let location = { physical_loc with
        loc_end = { physical_loc.loc_end with
          pos_lnum = physical_loc.loc_end.pos_lnum - 1;
          pos_cnum = physical_loc.loc_end.pos_cnum + 1;
      }} in
      COMMENT (line, location)
    }
  | "/*" ("*" "*"+)?
    { set_lexeme_length lexbuf 2;
      let start_loc = Location.curr lexbuf in
      Buffer.reset buffer;
      let {Location. loc_end; _} = comment buffer [start_loc] lexbuf in
      let text = Buffer.contents buffer in
      Buffer.reset buffer;
      COMMENT (text, { start_loc with Location.loc_end })
    }
  | "/**"
    { let start_p = lexbuf.Lexing.lex_start_p in
      let start_loc = Location.curr lexbuf in
      comment_start_loc := [start_loc];
      reset_string_buffer ();
      let _ = comment lexbuf in
      let s, _ = get_stored_string () in
      reset_string_buffer ();
      lexbuf.Lexing.lex_start_p <- start_p;
      DOCSTRING s
    }
  | "/**/"
    { DOCSTRING "" }
  | "/*/"
    { let loc = Location.curr lexbuf  in
      Location.prerr_warning loc Warnings.Comment_start;
      comment_start_loc := [loc];
      reset_string_buffer ();
      let {Location. loc_end; _} = comment lexbuf in
      let s, _ = get_stored_string () in
      reset_string_buffer ();
      COMMENT (s, { loc with Location.loc_end })
    }
  | "*/"
    { let loc = Location.curr lexbuf in
      Location.prerr_warning loc Warnings.Comment_not_end;
      set_lexeme_length lexbuf 1;
      STAR
    }
  | _ { assert false }

(* [comment buffer locs lexbuf] will lex a comment from [lexbuf] and
   stores its raw text in [buffer], without the /* and */ delimiters.
   [locs] is a non-empty list of locations that saves the beginning
   position of each nested comments.
 *)
and comment buffer locs = parse
  | "/*"
    { store_lexeme buffer lexbuf;
      comment buffer (Location.curr lexbuf :: locs) lexbuf;
    }
  | "*/"
    { match locs with
      | [] -> assert false
      | [_] -> Location.curr lexbuf
      | _ :: locs ->
         store_lexeme buffer lexbuf;
         comment buffer locs lexbuf;
    }
  | "\""
    { let start_loc = Location.curr lexbuf in
      Buffer.add_char buffer '"';
      begin
        try string true buffer lexbuf
        with Unterminated str_start ->
          let loc, start = list_first_last locs in
          raise_error
            (Unterminated_string_in_comment (start, str_start)) loc
      end;
      Buffer.add_char buffer '"';
      comment buffer locs lexbuf
    }
  | "{" (lowercase* as delim) "|"
    { store_lexeme buffer lexbuf;
      string_start_loc := Location.curr lexbuf;
      begin
        try quoted_string delim lexbuf
        with Unterminated str_start ->
          let loc, start = list_first_last locs in
          comment_start_loc := [];
          raise_error (Unterminated_string_in_comment (start, str_start)) loc
      end;
      store_string_char buffer '|';
      store_string delim;
      store_string_char buffer '}';
      comment buffer locs lexbuf
    }
  | "''"
    { store_lexeme buffer lexbuf;
      comment buffer locs lexbuf
    }
  | "'" newline "'"
    { update_loc lexbuf None 1 false 1;
      store_lexeme buffer lexbuf;
      comment lexbuf
    }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
      { store_lexeme string_buffer lexbuf; comment lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
      { store_lexeme string_buffer lexbuf; comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { store_lexeme string_buffer lexbuf; comment lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { store_lexeme string_buffer lexbuf; comment lexbuf }
  | eof
      { let loc, start = list_first_last !comment_start_loc in
        comment_start_loc := [];
        raise_error (Unterminated_comment start) loc;
        loc
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        store_lexeme string_buffer lexbuf;
        comment lexbuf
      }
  | _
      { store_lexeme string_buffer lexbuf; comment lexbuf }

and string context = parse
  | '"'
    { () }
  | '\\' newline ([' ' '\t'] * as space)
    { update_loc lexbuf None 1 false (String.length space);
      store_lexeme rawbuffer lexbuf;
      string context lexbuf
    }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
    { store_lexeme rawbuffer lexbuf;
      if not context.string_in_comment then
        store_string_char (char_for_backslash(Lexing.lexeme_char lexbuf 1));
      string context lexbuf
    }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
    { store_lexeme rawbuffer lexbuf;
      if not context.string_in_comment then
        store_string_char (char_for_decimal_code lexbuf 1);
      string context lexbuf
    }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
    { store_lexeme context.string_buffer lexbuf;
      if not context.string_in_comment then
        store_string_char(char_for_hexadecimal_code lexbuf 2);
      string context lexbuf
    }
  | '\\' _
    { store_lexeme rawbuffer lexbuf;
      if not context.string_in_comment then begin
        (*  FIXME: Warnings should probably go in Reason_errors
            Should be an error, but we are very lax.
              raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
                        Location.curr lexbuf))
           FIXME Using Location relies too much on compiler internals 
         *)
        Location.prerr_warning (Location.curr lexbuf)
          Warnings.Illegal_backslash;
        store_lexeme context.string_buffer lexbuf;
      end;
      string context lexbuf
    }
  | newline
    { store_lexeme context.string_raw_buffer lexbuf;
      if not context.string_in_comment then begin
        store_lexeme context.string_buffer lexbuf;
        Location.prerr_warning (Location.curr lexbuf) Warnings.Eol_in_string
      end;
      update_loc lexbuf None 1 false 0;
      string context lexbuf
    }
  | eof
    { if context.string_in_comment then
        raise (Unterminated context.string_start_loc);
      raise_error Unterminated_string context.string_start_loc
    }
  | _
    { store_lexeme rawbuffer lexbuf;
      if not context.string_in_comment then
        store_string_char (Lexing.lexeme_char lexbuf 0);
      string context lexbuf
    }

and quoted_string context = parse
  | newline
    { update_loc lexbuf None 1 false 0;
      store_lexeme context.string_buffer lexbuf;
      quoted_string context lexbuf
    }
  | eof
    { if context.string_in_comment then
        raise (Unterminated context.string_start_loc);
      raise_error Unterminated_string context.string_start_loc
    }
  | "|" (lowercase* as delim) "}"
    { if context.string_delim = delim then () else (
        store_lexeme context.string_buffer lexbuf;
        quoted_string context lexbuf
      )
    }
  | _ as c
    { Buffer.add_char context.string_buffer c;
      quoted_string context lexbuf
    }

and skip_sharp_bang = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
       { update_loc lexbuf None 3 false 0 }
  | "#!" [^ '\n']* '\n'
       { update_loc lexbuf None 1 false 0 }
  | "" { () }
