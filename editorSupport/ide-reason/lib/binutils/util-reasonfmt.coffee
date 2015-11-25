{BufferedProcess} = require 'atom'
path = require 'path'


# run stylish-haskell backend
prettify = (text, filePath, {onComplete, onFailure}) ->

  stdOutLines = []
  stdErrLines = []

  fmtPath = atom.config.get('ide-reason.pathToReasonfmt')
  if (!fmtPath)
    throw "No configuration set for ide-reason.pathToReasonfmt"

  printWidth = atom.config.get('ide-reason.printWidth') || 110

  proc = new BufferedProcess
    command: fmtPath
    args: ["-print-width", "" + printWidth, "-use-stdin", "true", "-parse", "re", "-print", "re", filePath]
    options:
      cwd: '.'
    stderr: (line) ->
      stdErrLines.push(line)
    stdout: (line) ->
      stdOutLines.push(line)
    exit: (code) -> onComplete?(code, stdOutLines.join(''), stdErrLines.join(''))

  proc.onWillThrowError ({error, handle}) ->
    atom.notifications.addError "ide-reason could not spawn #{fmtPath}",
      detail: "#{error}"
    console.error error
    onFailure?()
    handle()

  proc.process.stdin.write(text)
  proc.process.stdin.end()

module.exports = {
  prettify
}
