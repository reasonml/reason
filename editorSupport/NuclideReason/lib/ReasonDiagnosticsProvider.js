'use babel';
/* @flow */

/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the license found in the LICENSE file in
 * the root directory of this source tree.
 */

import type {
  DiagnosticProviderUpdate,
  MessageUpdateCallback,
  MessageInvalidationCallback,
} from 'nuclide/pkg/nuclide/diagnostics/base';
import {RE_GRAMMARS, RE_WORD_REGEX} from './constants';

import {DiagnosticsProviderBase} from 'nuclide/pkg/nuclide/diagnostics/provider-base';

import {Range} from 'atom';

const NuclideReason = require('../compiledSrc/jsBuild/app.js');

const PROVIDER_NAME = 'Merlin';

export default class ReasonDiagnosticsProvider {
  _providerBase: DiagnosticsProviderBase;

  constructor() {
    /**
     * Look here:
     * https://github.com/facebook/nuclide/blob/master/pkg/nuclide/diagnostics/provider-base/ for
     * a complete list of options.
     */
    const baseOptions = {
      shouldRunOnTheFly:  false,
      grammarScopes: new Set(['source.reason']),
      // This callback gets called whenever there is a text editor event, such as a file save, that
      // should trigger a run of diagnostics.
      onTextEditorEvent: editor => this._sendDiagnostics(editor),
      onNewUpdateSubscriber: callback => this._receivedNewUpdateSubscriber(callback)
    };
    this._providerBase = new DiagnosticsProviderBase(baseOptions);
    this._filePathToMessages = null;
    this._projectMessages = null;
  }

  _receivedNewUpdateSubscriber(callback: MessageUpdateCallback): void {
    // Every time we get a new subscriber, we need to push results to them. This
    // logic is common to all providers and should be abstracted out (t7813069)
    //
    // Once we provide all diagnostics, instead of just the current file, we can
    // probably remove the activeTextEditor parameter.
    const activeTextEditor = atom.workspace.getActiveTextEditor();
    if (activeTextEditor) {
      const matchesGrammar = RE_GRAMMARS.indexOf(activeTextEditor.getGrammar().scopeName) !== -1;
      if (matchesGrammar) {
        this._sendDiagnostics(activeTextEditor);
      }
    }
  }

  _sendDiagnostics(editor: TextEditor): void {
    const editorPath = editor.getPath();
    // When a New file is created, it will be "untitled" and getPath() will return null.
    if (editorPath == null) {
      return;
    }
    var preText = editor.getText();
    NuclideReason.getDiagnostics(preText)(editorPath)((diagnostics) => {
      const projectMessages = [];
      const fileMessagesByFilePathLookup = {};
      const fileMessagesByFilePath = [];
      diagnostics.forEach(d => {
        if (d.scope === 'project') {
          projectMessages.push(d);
        } else if (d.scope === 'file') {
          fileMessagesByFilePathLookup[d.filePath] =
            fileMessagesByFilePathLookup[d.filePath] || 
            (fileMessagesByFilePathLookup[d.filePath] = []);
          fileMessagesByFilePathLookup[d.filePath].push(d);
        }
      });
      for (var filePath in fileMessagesByFilePathLookup) {
        const messages = fileMessagesByFilePathLookup[filePath];
        fileMessagesByFilePath.push([filePath, messages]);
      }
      var filePathToMessages = new Map(fileMessagesByFilePath);
      const diagnosticsUpdate: DiagnosticProviderUpdate = {
        filePathToMessages: filePathToMessages,
        projectMessages: projectMessages,
      };
      this.invalidateProjectPath(editorPath);
      this._providerBase.publishMessageUpdate(diagnosticsUpdate);
      this._projectMessages = projectMessages;
      this._filePathToMessages = filePathToMessages;
    })();
  }

  /**
   * We have no way to know which project messages to  invalidate.
   */
  invalidateProjectPath(projectPath: string) {
    this._projectMessages = null;
    this._providerBase.publishMessageInvalidation({scope: 'project'});
    if (!this._filePathToMessages) {
      return;
    }
    const pathsToInvalidate = new Set();
    for (const itm of this._filePathToMessages) {
      const [filePath, messages] = itm;
      if (!filePath.startsWith(projectPath)) {
        continue;
      }
      pathsToInvalidate.add(filePath);
      this._filePathToMessages.delete(filePath);
    }
    // All project messages
    this._providerBase.publishMessageInvalidation({
      scope: 'file',
      filePaths: Array.from(pathsToInvalidate),
    });
  }

  // Delegate to these DiagnosticsProviderBase methods to satisfy the DiagnosticProvider interface.
  // These manage event subscriptions. A consumer of a diagnostics provider will subscribe to these
  // events. The DiagnosticsProviderBase takes care of the details of event subscription.
  onMessageUpdate(callback: MessageUpdateCallback): IDisposable {
    return this._providerBase.onMessageUpdate(callback);
  }

  onMessageInvalidation(callback: MessageInvalidationCallback): IDisposable {
    return this._providerBase.onMessageInvalidation(callback);
  }

  dispose() {
    this._providerBase.dispose();
  }
}

