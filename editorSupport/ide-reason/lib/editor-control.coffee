SubAtom = require 'sub-atom'

{bufferPositionFromMouseEvent} = require './utils'
{TooltipMessage} = require './views/tooltip-view'
{Range, Disposable, Emitter} = require 'atom'

class EditorControl
  constructor: (@editor) ->
    @disposables = new SubAtom
    @disposables.add @emitter = new Emitter

    editorElement = atom.views.getView(@editor)

    @gutter = @editor.gutterWithName "ide-reason-check-results"
    @gutter ?= @editor.addGutter
      name: "ide-reason-check-results"
      priority: 10

    gutterElement = atom.views.getView(@gutter)
    @disposables.add gutterElement, 'mouseenter', ".decoration", (e) =>
      bufferPt = bufferPositionFromMouseEvent @editor, e
      @lastMouseBufferPt = bufferPt
      @showCheckResult bufferPt, true
    @disposables.add gutterElement, 'mouseleave', ".decoration", (e) =>
      @hideTooltip()

    # buffer events for automatic check
    buffer = @editor.getBuffer()
    @disposables.add buffer.onWillSave =>
      @emitter.emit 'will-save-buffer', buffer
      if atom.config.get('ide-reason.onSavePrettify')
        atom.commands.dispatch editorElement, 'ide-reason:prettify-file'

    @disposables.add buffer.onDidSave =>
      @emitter.emit 'did-save-buffer', buffer

    @disposables.add @editor.onDidStopChanging =>
      @emitter.emit 'did-stop-changing', @editor

    @disposables.add editorElement.onDidChangeScrollLeft =>
      @hideTooltip eventType: 'mouse'
    @disposables.add editorElement.onDidChangeScrollTop =>
      @hideTooltip eventType: 'mouse'

    # show expression type if mouse stopped somewhere
    @disposables.add editorElement.rootElement, 'mousemove', '.scroll-view', (e) =>
      bufferPt = bufferPositionFromMouseEvent @editor, e

      return if @lastMouseBufferPt?.isEqual(bufferPt)
      @lastMouseBufferPt = bufferPt

      @clearExprTypeTimeout()
      @exprTypeTimeout = setTimeout (=> @shouldShowTooltip bufferPt),
        atom.config.get('ide-reason.expressionTypeInterval')
    @disposables.add editorElement.rootElement, 'mouseout', '.scroll-view', (e) =>
      @clearExprTypeTimeout()

    @disposables.add @editor.onDidChangeCursorPosition ({newBufferPosition}) =>
      switch atom.config.get('ide-reason.onCursorMove')
        when 'Show Tooltip'
          @clearExprTypeTimeout()
          unless @showCheckResult newBufferPosition, false, 'keyboard'
            @hideTooltip()
        when 'Hide Tooltip'
          @clearExprTypeTimeout()
          @hideTooltip()

  deactivate: ->
    @clearExprTypeTimeout()
    @hideTooltip()
    @disposables.dispose()
    @disposables = null
    @editor = null
    @lastMouseBufferPt = null

  # helper function to hide tooltip and stop timeout
  clearExprTypeTimeout: ->
    if @exprTypeTimeout?
      clearTimeout @exprTypeTimeout
      @exprTypeTimeout = null

  updateResults: (res, types) =>
    if types?
      for t in types
        m.destroy() for m in @editor.findMarkers {type: 'check-result', severity: t}
    else
      m.destroy() for m in @editor.findMarkers {type: 'check-result'}
    @markerFromCheckResult(r) for r in res

  markerFromCheckResult: ({uri, severity, message, position, positionEnd}) ->
    return unless uri? and uri is @editor.getURI()

    # create a new marker
    range = new Range position, positionEnd || {row: position.row, column: position.column + 1}
    marker = @editor.markBufferRange range,
      type: 'check-result'
      severity: severity
      desc: message

    @decorateMarker(marker)

  decorateMarker: (m) ->
    return unless @gutter?
    cls = 'ide-reason-' + m.getProperties().severity
    @gutter.decorateMarker m, type: 'line-number', class: cls
    @editor.decorateMarker m, type: 'highlight', class: cls
    @editor.decorateMarker m, type: 'line', class: cls

  onShouldShowTooltip: (callback) ->
    @emitter.on 'should-show-tooltip', callback

  onWillSaveBuffer: (callback) ->
    @emitter.on 'will-save-buffer', callback

  onDidSaveBuffer: (callback) ->
    @emitter.on 'did-save-buffer', callback

  onDidStopChanging: (callback) ->
    @emitter.on 'did-stop-changing', callback

  shouldShowTooltip: (pos) ->
    return if @showCheckResult pos

    if pos.row < 0 or
       pos.row >= @editor.getLineCount() or
       pos.isEqual @editor.bufferRangeForBufferRow(pos.row).end
      @hideTooltip eventType: 'mouse'
    else
      @emitter.emit 'should-show-tooltip', {@editor, pos}

  showTooltip: (pos, range, text, detail = {}) ->
    return unless @editor?

    if range.isEqual(@tooltipHighlightRange)
      return
    @hideTooltip()
    #exit if mouse moved away
    if detail.eventType is 'mouse'
      unless range.containsPoint(@lastMouseBufferPt)
        return
    @tooltipHighlightRange = range
    @markerBufferRange = range
    markerPos =
      switch detail.eventType
        when 'keyboard' then pos
        else range.start
    detail.type = 'tooltip'
    tooltipMarker = @editor.markBufferPosition markerPos, detail
    highlightMarker = @editor.markBufferRange range, detail
    @editor.decorateMarker tooltipMarker,
      type: 'overlay'
      item: new TooltipMessage text
    @editor.decorateMarker highlightMarker,
      type: 'highlight'
      class: 'ide-reason-type'

  hideTooltip: (template = {}) ->
    @tooltipHighlightRange = null
    template.type = 'tooltip'
    m.destroy() for m in @editor.findMarkers template

  getEventRange: (pos, eventType) ->
    switch eventType
      when 'mouse', 'context'
        pos ?= @lastMouseBufferPt
        [selRange] = @editor.getSelections()
          .map (sel) ->
            sel.getBufferRange()
          .filter (sel) ->
            sel.containsPoint pos
        crange = selRange ? Range.fromPointWithDelta(pos, 0, 0)
      when 'keyboard'
        crange = @editor.getLastSelection().getBufferRange()
        pos = crange.start
      else
        throw new Error "unknown event type #{eventType}"

    return {crange, pos}

  findCheckResultMarkers: (pos, gutter, keyboard) ->
    if gutter
      @editor.findMarkers {type: 'check-result', startBufferRow: pos.row}
    else if keyboard
      @editor.findMarkers {type: 'check-result', containsRange: Range.fromPointWithDelta pos, 0, 1}
    else
      @editor.findMarkers {type: 'check-result', containsPoint: pos}

  # show check result when mouse over gutter icon
  showCheckResult: (pos, gutter, eventType = 'mouse') ->
    markers = @findCheckResultMarkers pos, gutter, eventType is 'keyboard'
    [marker] = markers

    unless marker?
      @hideTooltip subtype: 'check-result'
      return false

    text = (markers.map (marker) ->
      marker.getProperties().desc).join('\n\n')

    if gutter
      @showTooltip pos, new Range(pos, pos), text, {eventType, subtype: 'check-result'}
    else
      @showTooltip pos, marker.getBufferRange(), text, {eventType, subtype: 'check-result'}

    return true

  hasTooltips: (template = {}) ->
    template.type = 'tooltip'
    !!@editor.findMarkers(template).length

module.exports = {
  EditorControl
}
