/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open NuclideReasonCommon;

export "getDiagnostics" (Js.wrap_callback NuclideReasonDiagnostics.getMerlinDiagnostics);

export "getFormatting" (Js.wrap_callback NuclideReasonFormat.getFormatting);

export
  "getNuclideJsAutocompleteSuggestions"
  (
    Js.wrap_callback (
      fun r => NuclideReasonAutoCompleteProvider.getNuclideJsAutocompleteSuggestions (
        NuclideJs.AutocompleteProviderRequest.fromJs r
      )
    )
  );

