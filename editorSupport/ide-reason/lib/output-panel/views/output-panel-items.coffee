OutputPanelItemElement = require './output-panel-item'

class OutputPanelItemsView extends HTMLElement
  setModel: (@model) ->

  createdCallback: ->
    @classList.add 'native-key-bindings'
    @setAttribute('tabindex', -1)
    @itemViews = []

  filter: (@activeFilter) ->
    scrollTop = @scrollTop
    @clear()
    @items = @model.filter @activeFilter
    @itemViews = for i in @items
      @appendChild (new OutputPanelItemElement).setModel i
    @scrollTop = scrollTop

  showItem: (item) ->
    view = @itemViews[@items.indexOf item]#atom.views.getView item
    return unless view?
    view.position.click()
    view.scrollIntoView
      block: "start"
      behavior: "smooth"

  scrollToEnd: ->
    @scrollTop = @scrollHeight

  atEnd: ->
    @scrollTop >= (@scrollHeight - @clientHeight)

  clear: ->
    i.destroy() for i in @itemViews
    @itemViews = []

  destroy: ->
    @remove()
    @clear()

OutputPanelItemsElement =
  document.registerElement 'ide-reason-panel-items',
    prototype: OutputPanelItemsView.prototype

module.exports = OutputPanelItemsElement
