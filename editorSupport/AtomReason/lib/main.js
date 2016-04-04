'use babel';
/* @flow */

/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the license found in the LICENSE file in
 * the root directory of this source tree.
 */

const Notiflyer = require('./Notiflyer');
const AtomReason = require('../output_byte_debug_js/app.js');
const {CompositeDisposable} = require('atom');

import {RE_GRAMMARS, RE_WORD_REGEX} from './constants';
const GRAMMARS_STRING = RE_GRAMMARS.join(', ');

const PACKAGE_NAME = 'AtomReason';

let reasonDiagnosticsProvider;
let disposables;

module.exports = {
  activate() {
    if (!disposables) {
      disposables = new CompositeDisposable();
    }
  },

  consumeStatusBar: (statusBar) => {
    statusBarTile = statusBar.addLeftTile({item: Notiflyer.feedbackBar, priority: 100});
  },

  /** Provider for autocomplete service. */
  createAutocompleteProvider(): atom$AutocompleteProvider {
    return {
      selector: '.source.reason',
      inclusionPriority: 1,
      disableForSelector: '.source.reason .comment',
      getSuggestions: AtomReason.getNuclideJsAutocompleteSuggestions,
      // We want to get ranked higher than the snippets provider.
      suggestionPriority: 5,
      onDidInsertSuggestion: () => {
      }
    };
  },

  getHyperclickProvider() {
    return {
      priority: 20,
      providerName: 'AtomReason',
      getSuggestionForWord: AtomReason.getLocation,
    };
  },

  provideLinter(): LinterProvider {
    const ReasonDiagnosticsProvider = require('./ReasonDiagnosticsProvider');
    return ReasonDiagnosticsProvider;
  },

  createCodeFormatProvider() {
    return {
      selector: GRAMMARS_STRING,
      inclusionPriority: 1,
      formatEntireFile(editor: atom$TextEditor, range: atom$Range): Promise<string> {
        return AtomReason.getEntireFormatting(
          editor,
          range,
          Notiflyer.showSuccesBar,
          Notiflyer.showFailBar,
          Notiflyer.showInfoBar,
        );
      },
      formatCode(editor: atom$TextEditor, range: atom$Range): Promise<string> {
        return AtomReason.getPartialFormatting(
          editor,
          range,
          Notiflyer.showSuccesBar,
          Notiflyer.showFailBar,
          Notiflyer.showInfoBar,
        );
      },
    };
  },

  // Note that we're actually creating a nuclide-datatip provider, not
  // nuclide-type-hint provider. We historically used type-hint provider, but it
  // doesn't display rich content as of this diff. We'll switch back when I get
  // bored enough to submit a PR against nuclide.
  createTypeHintProvider() {
    return {
      validForScope: scope => scope === 'source.reason',
      providerName: PACKAGE_NAME,
      inclusionPriority: 1,
      datatip: AtomReason.getNuclideJsTypeHint,
    };
  },

  deactivate() {
    statusBarTile && statusBarTile.destroy();
    statusBarTile = null;

    // TODO: Figure out how to dispose of the merlin service that had spawned via SuperMerlin.re.
    if (disposables) {
      disposables.dispose();
      disposables = null;
    }
    if (reasonDiagnosticsProvider) {
      reasonDiagnosticsProvider.dispose();
      reasonDiagnosticsProvider = null;
    }
  },
  config: {
    "pathToReasonfmt": {
      "title": "Path To Reasonfmt",
      "type": "string",
      "default": "reasonfmt",
      "description":
        "Absolute path of `reasonfmt` binary - Reason syntax source formatter."
    },
    "pathToRefmttype": {
      "title": "Path To Refmttype",
      "type": "string",
      "default": "refmttype",
      "description":
        "Absolute path of `refmttype` binary - Ocaml type to Reason type formatter."
    },
    "printWidth": {
      "title": "Default Print Width of Reasonfmt",
      "type": "number",
      "default": 110,
      "description": "Default line wrapping width for pretty printing"
    },
    "pathToMerlin": {
      "title": "Path To Merlin",
      "type": "string",
      "default": "ocamlmerlin",
      "description":
        "Absolute path of `ocamlmerlin` binary, which may override other plugins' settings for merlin paths (Nuclide for example)"
    },
    "merlinFlags": {
      "title": "Flags Passed To Merlin Executable",
      "type": "string",
      "default": "-pp reasonfmt_merlin",
      "description":
        "Flags to pass to the Merlin executable - important for configuring Reason syntax."
    },
    "merlinLogFile": {
      "title": "Absolute File Path For Writing Merlin Log",
      "type": "string",
      "default": "",
      "description":
        "Destination file (such as /Users/you/merlinLog.txt) that Merlin should log to. Requires Atom restart."
    },
  }

};
