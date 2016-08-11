/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
type merlinCompletionEntryKind =
  | Value | Variant | Constructor | Label | Module | Signature | Type | Method;

type merlinCompletionEntry = {
  desc: string,
  info: string,
  kind: merlinCompletionEntryKind,
  name: string
};

type merlinLocateEntryCurrentFile = {position: Atom.Range.t};

type merlinLocateEntryAnotherFile = {position: Atom.Range.t, file: string};

type merlinLocateEntry =
  | InvalidIdentifier
  | OtherError string
  | CurrentFile merlinLocateEntryCurrentFile
  | AnotherFile merlinLocateEntryAnotherFile;
