SubAtom = require 'sub-atom'

ansi_up = require "ansi_up"

class OutputPanelItemView extends HTMLElement
  setModel: (@model) ->
    @innerHTML = ''
    if @model.uri? and @model.position?
      @appendChild @position = document.createElement 'ide-reason-item-position'
      @position.innerText = "#{@model.uri}:#{@model.position.row + 1} (line #{@model.position.column + 1})"
    @appendChild @description = document.createElement 'ide-reason-item-description'
    @description.innerHTML = ansi_up.ansi_to_html ansi_up.escape_for_html(@model.message),
      use_classes:true
    @

  attachedCallback: ->
    @disposables = new SubAtom
    if @position?
      @disposables.add @position, 'click', =>
        atom.workspace.open(@model.uri).then (editor) =>
          editor.setCursorBufferPosition @model.position

  destroy: ->
    @remove()
    @disposables.dispose()
    @disposables = null


OutputPanelItemElement =
  document.registerElement 'ide-reason-panel-item',
    prototype: OutputPanelItemView.prototype

module.exports = OutputPanelItemElement
