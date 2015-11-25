OutputPanel = require './output-panel/output-panel'
{EditorControl} = require './editor-control'
{TooltipMessage} = require './views/tooltip-view'
ResultsDB = require './results-db'
ResultItem = require './result-item'
{CompositeDisposable, Emitter} = require 'atom'
{dirname} = require 'path'
{statSync} = require 'fs'

class PluginManager
  constructor: (state) ->
    @checkResults = new ResultsDB

    @disposables = new CompositeDisposable
    @controllers = new WeakMap
    @disposables.add @emitter = new Emitter

    @disposables.add @onResultsUpdated ({types}) => @updateEditorsWithResults(types)

    @createOutputViewPanel(state)
    @subscribeEditorController()

  deactivate: ->
    @checkResults.destroy()
    @disposables.dispose()
    @backend?.shutdownBackend?()

    @deleteEditorControllers()
    @deleteOutputViewPanel()

  serialize: ->
    outputView: @outputView?.serialize()

  onShouldShowTooltip: (callback) ->
    @emitter.on 'should-show-tooltip', callback

  onWillSaveBuffer: (callback) ->
    @emitter.on 'will-save-buffer', callback

  onDidSaveBuffer: (callback) ->
    @emitter.on 'did-save-buffer', callback

  onDidStopChanging: (callback) ->
    @emitter.on 'did-stop-changing', callback

  togglePanel: ->
    @outputView?.toggle()

  updateEditorsWithResults: (types) ->
    for ed in atom.workspace.getTextEditors()
      @controller(ed)?.updateResults?(@checkResults.filter uri: ed.getPath(), types)

  onResultsUpdated: (callback) =>
    @checkResults.onDidUpdate callback

  controller: (editor) ->
    @controllers?.get? editor

  # Create and delete output view panel.
  createOutputViewPanel: (state) ->
    @outputView = new OutputPanel(state.outputView, @checkResults)

  deleteOutputViewPanel: ->
    @outputView.destroy()
    @outputView = null

  addController: (editor) ->
    unless @controllers.get(editor)?
      @controllers.set editor, controller = new EditorControl(editor)
      controller.disposables.add editor.onDidDestroy =>
        @removeController editor
      controller.disposables.add controller.onShouldShowTooltip ({editor, pos}) =>
        @emitter.emit 'should-show-tooltip', {editor, pos, eventType: 'mouse'}
      controller.disposables.add controller.onWillSaveBuffer (buffer) =>
        @emitter.emit 'will-save-buffer', buffer
      controller.disposables.add controller.onDidSaveBuffer (buffer) =>
        @emitter.emit 'did-save-buffer', buffer
      controller.disposables.add controller.onDidStopChanging (editor) =>
        @emitter.emit 'did-stop-changing', editor
      controller.updateResults @checkResults.filter uri: editor.getPath()

  removeController: (editor) ->
    @controllers.get(editor)?.deactivate()
    @controllers.delete(editor)

  controllerOnGrammar: (editor, grammar) ->
    if grammar.scopeName.match /reason$/
      @addController editor
    else
      @removeController editor

  # Observe text editors to attach controller
  subscribeEditorController: ->
    @disposables.add atom.workspace.observeTextEditors (editor) =>
      @disposables.add editor.onDidChangeGrammar (grammar) =>
        @controllerOnGrammar editor, grammar
      @controllerOnGrammar editor, editor.getGrammar()

  deleteEditorControllers: ->
    for editor in atom.workspace.getTextEditors()
      @removeController editor

  nextError: ->
    @outputView?.showNextError()

  prevError: ->
    @outputView?.showPrevError()


module.exports = {
  PluginManager
}
