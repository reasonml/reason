'use babel';
/* @flow */

/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the license found in the LICENSE file in
 * the root directory of this source tree.
 */

const GRAMMARS = new Set([
  'source.reason',
]);

const EXTENSIONS = {
  're': 'ml',
  'rei': 'mli'
};

module.exports = {
  priority: 20,
  providerName: 'NuclideReason',
  async getSuggestionForWord(textEditor: TextEditor, text: string, range: atom$Range) {
    const {getServiceByNuclideUri} = require('nuclide/pkg/nuclide-client');

    if (!GRAMMARS.has(textEditor.getGrammar().scopeName)) {
      return null;
    }

    const file = textEditor.getPath();

    let kind = 'ml';
    const extension = require('path').extname(file);
    if (EXTENSIONS[extension]) {
      kind = extension;
    }

    const instance = await getServiceByNuclideUri('MerlinService', file);
    const start = range.start;

    return {
      range,
      callback: async function() {
        await instance.pushNewBuffer(file, textEditor.getText());
        const location = await instance.locate(
          file,
          start.row,
          start.column,
          kind
        );
        if (!location) {
          return;
        }

        const {goToLocation} = require('nuclide/pkg/nuclide-atom-helpers');
        goToLocation(location.file, location.pos.line - 1, location.pos.col);
      },
    };
  },
};
