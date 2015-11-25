path = require 'path'

module.exports = Utils =
  MainMenuLabel: 'Reason IDE'

  getEventType: (detail) ->
    if detail?.contextCommand?
      'context'
    else
      'keyboard'

  # screen position from mouse event
  bufferPositionFromMouseEvent: (editor, event) ->
    editor.bufferPositionForScreenPosition (
      atom.views.getView(editor).component.screenPositionForMouseEvent event)
