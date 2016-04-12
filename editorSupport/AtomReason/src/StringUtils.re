/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
let (\===) a b => String.compare a b === 0;

let startsWith str searchFor => {
  let searchForLen = String.length searchFor;
  let strLen = String.length str;
  searchForLen <= strLen && String.sub str 0 searchForLen \=== searchFor
};

let module Regex = {
  type t;
  let create (s: string) :t => Js.Unsafe.new_obj "RegExp" [|Js.Unsafe.inject (Js.string s)|];
};

let replace (s: string) (r: Regex.t) (replaceWith: string) :string => Js.to_string (
  Js.Unsafe.meth_call
    (Js.string s) "replace" [|Js.Unsafe.inject r, Js.Unsafe.inject (Js.string replaceWith)|]
);

let split by::(r: Regex.t) (s: string) => {
  let s = Js.string s;
  let jsArr = Js.Unsafe.meth_call s "split" [|Js.Unsafe.inject r|];
  let lst = Array.to_list (Js.to_array jsArr);
  List.map (fun itm => Js.to_string itm) lst
};
