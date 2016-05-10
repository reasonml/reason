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

const PACKAGE_NAME = 'atom-reason';

let reasonDiagnosticsProvider;
let disposables;

module.exports = {
  activate() {
    if (!disposables) {
      disposables = new CompositeDisposable();

      disposables.add(atom.commands.add('atom-workspace', 'atom-reason:select-in-scope', function() {
        var editor = atom.workspace.getActiveTextEditor();
        // sometimes there's no active editor, e.g. we're on the Settings page.
        if (editor) {
          AtomReason.selectOccurrences(editor);
        }
      }));
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
      providerName: PACKAGE_NAME,
      getSuggestionForWord: AtomReason.getLocation,
    };
  },

  provideLinter(): LinterProvider {
    return {
      name: 'atom-reason',
      grammarScopes: RE_GRAMMARS,
      scope: 'file',
      lintOnFly: true,
      lint: AtomReason.getDiagnostics,
    };
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

  createTypeHintProvider() {
    return {
      selector: 'source.reason',
      providerName: PACKAGE_NAME,
      inclusionPriority: 1,
      typeHint: AtomReason.getNuclideJsTypeHint,
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
    "pathToRefmt": {
      "title": "Path To Refmt",
      "type": "string",
      "default": "refmt",
      "description":
        "Absolute path of `refmt` binary - Reason syntax source formatter."
    },
    "pathToRefmttype": {
      "title": "Path To Refmttype",
      "type": "string",
      "default": "refmttype",
      "description":
        "Absolute path of `refmttype` binary - Ocaml type to Reason type formatter."
    },
    "printWidth": {
      "title": "Default Print Width of Refmt",
      "type": "number",
      "default": 0,
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
      "default": "",
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
