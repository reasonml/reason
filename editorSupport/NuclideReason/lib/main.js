'use babel';
/* @flow */

/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the license found in the LICENSE file in
 * the root directory of this source tree.
 */

import type {HyperclickProvider} from 'nuclide/pkg/nuclide/hyperclick-interfaces';
import type {
  BusySignalProviderBase as BusySignalProviderBaseType,
} from 'nuclide/pkg/nuclide/busy-signal-provider-base';

const Notiflyer = require('./Notiflyer');
const NuclideReason = require('../compiledSrc/jsBuild/app.js');
const invariant = require('assert');
const {CompositeDisposable} = require('atom');

import {RE_GRAMMARS, RE_WORD_REGEX} from './constants';
const GRAMMARS_STRING = RE_GRAMMARS.join(', ');

const PACKAGE_NAME = 'NuclideReason';

function getServiceByNuclideUri(service, file?) {
  return require('nuclide/pkg/nuclide/client').getServiceByNuclideUri(service, file);
}

let busySignalProvider;
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
      getSuggestions: NuclideReason.getNuclideJsAutocompleteSuggestions,
      // We want to get ranked higher than the snippets provider.
      suggestionPriority: 5,
      onDidInsertSuggestion: () => {
      }
    };
  },

  getHyperclickProvider(): HyperclickProvider {
    return require('./ReasonHyperclickProvider');
  },

  provideBusySignal(): BusySignalProviderBaseType {
    if (!busySignalProvider) {
      const {DedupedBusySignalProviderBase} = require('nuclide/pkg/nuclide/busy-signal-provider-base');
      busySignalProvider = new DedupedBusySignalProviderBase();
    }
    return busySignalProvider;
  },

  provideDiagnostics() {
    if (!reasonDiagnosticsProvider) {
      const ReasonDiagnosticsProvider = require('./ReasonDiagnosticsProvider');
      reasonDiagnosticsProvider = new ReasonDiagnosticsProvider();
    }
    const {projects} = require('nuclide/pkg/nuclide/atom-helpers');
    disposables.add(projects.onDidRemoveProjectPath(projectPath => {
      reasonDiagnosticsProvider.invalidateProjectPath(projectPath);
    }));
    
    return reasonDiagnosticsProvider;
  },
  
  createCodeFormatProvider() {
    return {
      selector: GRAMMARS_STRING,
      inclusionPriority: 1,
      formatEntireFile(editor: atom$TextEditor, range: atom$Range): Promise<string> {
        return NuclideReason.getFormatting(editor, range, Notiflyer.showSuccesBar, Notiflyer.showFailBar, Notiflyer.showInfoBar);
      },
    };
  },

  // createTypeHintProvider(): Object {
  //   const {FlowTypeHintProvider} = require('./FlowTypeHintProvider');
  //   const flowTypeHintProvider = new FlowTypeHintProvider();
  //   const typeHint = flowTypeHintProvider.typeHint.bind(flowTypeHintProvider);
  //   return {
  //     selector: GRAMMARS_STRING,
  //     providerName: PACKAGE_NAME,
  //     inclusionPriority: 1,
  //     typeHint,
  //   };
  // },
  //
  deactivate() {
    statusBarTile && statusBarTile.destroy();
    statusBarTile = null;

    // TODO: Figure out how to dispose of the merlin service that had spawned via MerlinService.re.
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
  }

};
