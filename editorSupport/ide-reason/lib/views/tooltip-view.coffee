class TooltipMessage
  constructor: (text) ->
    @element = (new TooltipElement).setMessage text

class TooltipView extends HTMLElement
  setMessage: (message) ->
    @inner.innerHTML = ansi_up.ansi_to_html ansi_up.escape_for_html(message),
      use_classes:true
    @

  createdCallback: ->
    @appendChild @inner = document.createElement 'div'

  attachedCallback: ->
    @parentElement.classList.add 'ide-reason'

TooltipElement =
  document.registerElement 'ide-reason-tooltip',
    prototype: TooltipView.prototype

module.exports = {
  TooltipMessage
}
