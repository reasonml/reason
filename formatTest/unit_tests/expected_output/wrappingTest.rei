/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
let named: a::int => b::int => int;
let namedAlias: a::int => b::int => int;
let namedAnnot: a::option int => b::option int => int;
let namedAliasAnnot: a::option int => b::option int => int;
let optional: a::'a? => b::'b? => unit => int;
let optionalAlias: a::'a? => b::'b? => unit => int;
let optionalAnnot: a::int? => b::int? => unit => int;
let optionalAliasAnnot: a::int? => b::int? => unit => int;
let defOptional: a::int? => b::int? => unit => int;
let defOptionalAlias: a::int? => b::int? => unit => int;
let defOptionalAnnot: a::int? => b::int? => unit => int;
let defOptionalAliasAnnot: a::int? => b::int? => unit => int;
let fun_option_int: option int => option int => int;