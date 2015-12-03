{BufferedProcess} = require 'atom'
nuclideCommons = require 'nuclide-commons'
getReasonifyConfig = require '../getReasonifyConfig'
nuclideClient = require 'nuclide-client'
getServiceByNuclideUri = nuclideClient.getServiceByNuclideUri


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

  performCompile = (buildFlagsResult, paths) ->
    flags = buildFlagsResult && buildFlagsResult['.merlin'] && buildFlagsResult['.merlin'].slice(0) || []
    pathFlags = (paths || []).forEach (p) ->
      flags.push '-I'
      flags.push p
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
          args: ['-pp', reasonifyConfig.pathToReasonfmt].concat(flags).concat(['-c', '-o', outputPath, kindFlag, filePath])
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


  merlinService = getServiceByNuclideUri 'MerlinService', filePath
  (merlinService._getInstance (filePath)).then (merlinService) ->
    buildFlagsResult = null
    buildPathsResult = null
    buildFlags = merlinService.runSingleCommand ["dump", "flags"]
    buildFlags.then (flags) ->
      buildFlagsResult = flags
      buildPaths = merlinService.runSingleCommand ["path", "list", "build"]
      buildPaths.then (paths) ->
        buildPathsResult = paths
        performCompile buildFlagsResult, buildPathsResult



  temp.cleanup (err, stats) ->
    ## This actually doesn't seem to be cleaning these up

module.exports = {
  compile
}
