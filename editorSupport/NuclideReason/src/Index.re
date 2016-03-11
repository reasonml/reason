/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
/*
 * Ideally, most of NuclideReason would be in pure Reason, and the parts that need to
 * to convert to/from JS would be at the edges (Index.re, and MerlinService.re).
 */
open NuclideReasonCommon;

export
  "getDiagnostics"
  (
    Js.wrap_callback (
      fun text path onComplete onFailure => {
        let onComplete arr => Js.Unsafe.fun_call onComplete [|Js.Unsafe.inject (Js.array arr)|];
        let onFailure str => Js.Unsafe.fun_call onComplete [|Js.Unsafe.inject (Js.string str)|];
        NuclideReasonDiagnostics.getMerlinDiagnostics text path onComplete onFailure
      }
    )
  );

export
  "getFormatting"
  (
    Js.wrap_callback (
      fun jsEditor jsRange jsNotifySuccess jsNotifyInvalid jsNotifyInfo => {
        let editor = Atom.Editor.fromJs jsEditor;
        let range = Atom.Range.fromJs jsRange;
        let notifySuccess msg => Js.Unsafe.fun_call jsNotifySuccess [|Js.Unsafe.inject (Js.string msg)|];
        let notifyInvalid msg => Js.Unsafe.fun_call jsNotifyInvalid [|Js.Unsafe.inject (Js.string msg)|];
        let notifyInfo msg => Js.Unsafe.fun_call jsNotifyInfo [|Js.Unsafe.inject (Js.string msg)|];
        let promise = NuclideReasonFormat.getFormatting editor range notifySuccess notifyInvalid notifyInfo;
        Atom.Promise.toJs promise
      }
    )
  );

export
  "getNuclideJsAutocompleteSuggestions"
  (
    Js.wrap_callback (
      fun r => NuclideReasonAutoCompleteProvider.getNuclideJsAutocompleteSuggestions (
        NuclideJs.AutocompleteProviderRequest.fromJs r
      )
    )
  );

