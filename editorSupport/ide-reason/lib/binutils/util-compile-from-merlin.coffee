{BufferedProcess} = require 'atom'
nuclideCommons = require 'nuclide-commons'
getReasonifyConfig = require '../getReasonifyConfig'


path = require 'path'
fs   = require 'fs'
temp = require 'temp'


compile = (text, filePath, {onComplete, onFailure}) ->
  stdOutLines = []
  stdErrLines = []

  reasonifyConfig = getReasonifyConfig()
  if (!reasonifyConfig.pathToReasonfmt)
    throw "Cannot compile because no value specified for config ide-reason.pathToReasonfmt"

  compilerPath = atom.config.get('ide-reason.pathToCompiler')
  if (!compilerPath)
    throw "Cannot compile because no value specified for config ide-reason.pathToCompiler"

  getMerlinFlags = (merlinContents) ->
    lines = merlinContents.split('\n')
    buildFlags = []
    lines.forEach ((line) ->
      match = line.match (/^B\s+(\S*)/)
      if match && match[1]
        buildFlags.push('-I')
        buildFlags.push(match[1])
    )
    lines.forEach ((line) ->
      match = line.match (/^FLG\s+([\S\s]*)/)
      if match && match[1]
        buildFlags.push.apply buildFlags, match[1].split(/\s+/)
    )
    buildFlags
  performCompile = (compileFlagsFromMerlin) ->
    extension = path.extname(filePath)
    basename = path.basename(filePath)
    # We'll not lock ourselves into any extension scheme other than
    # assuming that ending in i means interfae, impl otherwise
    isInterface = extension[extension.length - 1] == 'i'
    temp.track()
    temp.mkdir 'temporaryCompilerArtifacts', (err, dirPath) ->
      # Normally, we'd do an fs.writeFile on the inputPath to copy over the buffer
      # contents, but that would ruin file names in error messages. Here, instead,
      # we count on this only being invoked after saving.
      inputPath = path.join dirPath, basename
      outputPath = inputPath + '.out'
      if err
        throw err
      else
        kindFlag =
          if isInterface
            '-intf'
          else
            '-impl'
        proc = new BufferedProcess
          command: compilerPath
          args: ['-pp', reasonifyConfig.pathToReasonfmt].concat(compileFlagsFromMerlin).concat(['-c', '-o', outputPath, kindFlag, filePath])
          options:
            cwd: '.'
          stderr: (line) ->
            stdErrLines.push(line)
          stdout: (line) ->
            stdOutLines.push(line)
          exit: (code) -> onComplete?(code, stdOutLines.join(''), stdErrLines.join(''))

        proc.onWillThrowError ({error, handle}) ->
          atom.notifications.addError "Ide-Reason could not spawn #{compilerPath} with #{reasonifyConfig.pathToReasonfmt}. Check your paths",
            detail: "#{error}"
          console.error error
          onFailure?()
          handle()

        proc.process.stdin.end()

  nearestMerlin = nuclideCommons.findNearestFile '.merlin', filePath
  nearestMerlin.then ((nearest) ->
    if nearest == null
      performCompile []
    else
      fs.readFile (path.join nearest, ".merlin"), (err, merlinContents) ->
        if err
          throw err
        performCompile (getMerlinFlags (merlinContents.toString()))
  )

  temp.cleanup (err, stats) ->
    ## This actually doesn't seem to be cleaning these up
  
module.exports = {
  compile
}
