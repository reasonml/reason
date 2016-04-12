/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
type merlinCompletionEntryKind = | Value | Variant | Constructor | Label | Module | Signature | Type | Method;

type merlinCompletionEntry = {desc: string, info: string, kind: merlinCompletionEntryKind, name: string};
