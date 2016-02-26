(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

let reasonFormatter = Reason_pprint_ast.createFormatter ()

let customIncompatibleTypeForReason errorBody cachedContent range =
  let open BetterErrorsTypes in
  match BetterErrorsParseError.type_IncompatibleType errorBody cachedContent range with
  | Type_IncompatibleType {actual; expected; actualEquivalentType; expectedEquivalentType; extra} ->
    let actual = Reason_type_of_ocaml_type.convert actual in
    let expected = Reason_type_of_ocaml_type.convert expected in
    Type_IncompatibleType {
      actual;
      expected;
      (* we're using the error reporter's typeDiff logic temporarily. That diff
      is semantic, aka knows about module boundaries from dot, and function
      argument boundaries from ->. But since reason uses => as function
      delimiter we'll need to customize the diff function soon *)
      differingPortion = BetterErrorsParseError.typeDiff actual expected;
      actualEquivalentType;
      expectedEquivalentType;
      extra;
    }
  (* currently the error reporter logic asks you to throw in order to pass onto
  the next parser that tries to parse the error. This branch, however, will
  never be reached because type_IncompatibleType always returns the
  `Type_IncompatibleType blaRecord` variant *)
  | _ -> raise Not_found

let () =
  BetterErrorsMain.parseFromStdin
    ~customErrorParsers:[customIncompatibleTypeForReason]
