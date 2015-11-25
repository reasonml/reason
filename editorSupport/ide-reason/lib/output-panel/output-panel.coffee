OutputPanelElement = require './views/output-panel'
{CompositeDisposable, Emitter} = require 'atom'

module.exports=
class OutputPanel
  constructor: (@state = {}, @results) ->
    @disposables = new CompositeDisposable

    pos = atom.config.get('ide-reason.panelPosition')

    @element = (new OutputPanelElement).setModel @
    @element.setPanelPosition pos

    @panel = atom.workspace.addPanel pos,
      item: @
      visible: @state?.visibility ? true

    atom.config.onDidChange 'ide-reason.panelPosition', ({newValue}) =>
      @element.setPanelPosition newValue
      atom.workspace.addPanel newValue, item: @

    @disposables.add @results.onDidUpdate => @currentResult = null

    @backendStatus status: 'ready'

  toggle: ->
    if @panel.isVisible()
      @panel.hide()
    else
      @panel.show()

  destroy: ->
    @disposables.dispose()
    @panel.destroy()
    @element.destroy()

  createTab: (name, opts) ->
    @element.createTab name, opts

  serialize: ->
    visibility: @panel.isVisible()
    height: @element.style.height
    width: @element.style.width
    activeTab: @element.getActiveTab()
    fileFilter: @element.buttons.getFileFilter()

  addPanelControl: (element, opts) ->
    @element.addPanelControl element, opts

  backendStatus: ({status, progress}) ->
    @element.statusChanged {status, oldStatus: @status ? 'ready'}
    @status = status
    unless status is 'progress'
      progress ?= 0
    @element.setProgress progress if progress?

  showNextError: ->
    rs = @results.resultsWithURI()
    return if rs.length is 0

    if @currentResult?
      @currentResult++
    else
      @currentResult = 0
    @currentResult = 0 if @currentResult >= rs.length

    @element.showItem rs[@currentResult]

  showPrevError: ->
    rs = @results.resultsWithURI()
    return if rs.length is 0

    if @currentResult?
      @currentResult--
    else
      @currentResult = rs.length - 1
    @currentResult = rs.length - 1 if @currentResult < 0

    @element.showItem rs[@currentResult]
