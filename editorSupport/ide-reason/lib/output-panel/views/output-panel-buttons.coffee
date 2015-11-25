{Emitter} = require 'atom'
SubAtom = require 'sub-atom'

module.exports=
class OutputPanelButtons extends HTMLElement
  createdCallback: ->
    @disposables = new SubAtom
    @disposables.add @emitter = new Emitter
    @buttons = {}
    @appendChild @buttonsContainer = document.createElement 'ide-reason-buttons-container'
    ['error', 'warning', 'lint'].forEach (btn) =>
      @createButton btn
    @createButton 'build',
      uriFilter: false
      autoScroll: true
    @appendChild @cbCurrentFile = document.createElement 'ide-reason-checkbox'
    @disposables.add @cbCurrentFile, 'click', => @toggleFileFilter()

  createButton: (btn, opts) ->
    @buttons[btn] =
      element: null
      options: opts ? {}
    @buttonsContainer.appendChild @buttons[btn].element = document.createElement 'ide-reason-button'
    @buttons[btn].element.setAttribute 'data-caption', btn
    @buttons[btn].element.setAttribute 'data-count', 0
    @disposables.add @buttons[btn].element, 'click', => @clickButton btn

  options: (btn) ->
    opts = if @buttons[btn]?
      @buttons[btn].options
    else
      {}
    opts['uriFilter'] ?= true
    opts['autoScroll'] ?= false
    opts

  onButtonClicked: (callback) ->
    @emitter.on 'button-clicked', callback

  onCheckboxSwitched: (callback) ->
    @emitter.on 'checkbox-switched', callback

  buttonNames: ->
    Object.keys @buttons

  clickButton: (btn) ->
    if @buttons[btn]?
      for v in @getElementsByClassName 'active'
        v.classList.remove 'active'
      @buttons[btn].element.classList.add 'active'
      @emitter.emit 'button-clicked', btn

  setFileFilter: (state) ->
    if state
      @cbCurrentFile.classList.add 'enabled'
      @emitter.emit 'checkbox-switched', true
    else
      @cbCurrentFile.classList.remove 'enabled'
      @emitter.emit 'checkbox-switched', false

  getFileFilter: ->
    @cbCurrentFile.classList.contains 'enabled'

  toggleFileFilter: ->
    @setFileFilter not @getFileFilter()

  setCount: (btn, count) ->
    if @buttons[btn]?
      @buttons[btn].element.setAttribute 'data-count', count

  destroy: ->
    @remove()
    @disposables.dispose()

  getActive: ->
    @getElementsByClassName('active')[0]?.getAttribute?('data-caption')

OutputPanelButtonsElement =
  document.registerElement 'ide-reason-panel-buttons',
    prototype: OutputPanelButtons.prototype

module.exports = OutputPanelButtonsElement
