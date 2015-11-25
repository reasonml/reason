utilReasonfmt = require './util-reasonfmt'
utilCompileFromMerlin = require './util-compile-from-merlin'
extractDiagnostics = require 'CommonML/extractDiagnostics'
formatErrorMessages = require '../../Reasonify/formatErrorMessages'
path = require 'path'

module.exports = ToolChain =
  validateParse: (editor, upi) ->
    editorPath = editor.getPath()
    preText = editor.getText()
    utilReasonfmt.prettify preText, editorPath,
      onFailure: (err) ->
        debugger
      onComplete: (code, stdOutText, stdErrText) ->
        if code != 0
          ToolChain.messageStandardError stdErrText, upi
  validateCompile: (editor, upi) ->
    editorPath = editor.getPath()
    preText = editor.getText()
    utilCompileFromMerlin.compile preText, editorPath,
      onFailure: (err) ->
        debugger
      onComplete: (code, stdOutText, stdErrText) ->
        # Warnings still result in code 0, but stdErrText
        if code != 0 || stdErrText != ''
          ToolChain.messageStandardError stdErrText, upi

  prettifyFile: (editor, upi) ->
    [firstCursor, cursors...] = editor.getCursors().map (cursor) ->
      cursor.getBufferPosition()
    editorPath = editor.getPath()
    preText = editor.getText()
    utilReasonfmt.prettify preText, editorPath,
      onFailure: (err) ->
        debugger
      onComplete: (code, stdOutText, stdErrText) ->
        if code == 0
          nextText = (line.trimRight() for line in stdOutText.split('\n')).join('\n')
          if preText != nextText
            # Avoids screwing up undo/redo cursor position as much as possible.
            editor.getBuffer().setTextViaDiff(nextText)
            if editor.getLastCursor()?
              editor.getLastCursor().setBufferPosition firstCursor,
                autoscroll: false
              cursors.forEach (cursor) ->
                editor.addCursorAtBufferPosition cursor,
                  autoscroll: false
        else
          ToolChain.messageStandardError stdErrText, upi
  messageStandardError: (stdErrText, upi) ->
    diagnostics = extractDiagnostics.extractFromStdErr("prettify", stdErrText, false)
    messages = diagnostics.map (diagnostic) ->
      uri:diagnostic.filePath
      position:
        # ide-reason has zero based rows
        row:diagnostic.range[0][0] - 1
        column:diagnostic.range[0][1]
      positionEnd:
        # ide-reason has zero based rows
        row:diagnostic.range[1][0] - 1
        column:diagnostic.range[1][1]
      message: formatErrorMessages [diagnostic]
      severity:
        if diagnostic.type == 'ERROR'
          'error'
        else if diagnostic.type == 'WARNING'
          'warning'
        else
          'error'
    upi.setMessages messages
  parsePrint: (editor, upi, shouldPrettify) ->
