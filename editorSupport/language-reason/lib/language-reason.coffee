# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
LanguageReasonView = require './language-reason-view'
{CompositeDisposable} = require 'atom'

module.exports = LanguageReason =
  languageReasonView: null
  modalPanel: null
  subscriptions: null

  activate: (state) ->
    # Acount for faulty package loading when restoring session
    # https://github.com/atom/atom/issues/8313
    lps = atom.workspace.getTextEditors().map((ed) ->
        ed.getGrammar().packageName
      )
    if (lps.indexOf('language-reason') != -1)
      atom.packages.triggerActivationHook('language-reason:grammar-used')

    @languageReasonView = new LanguageReasonView(state.languageReasonViewState)
    @modalPanel = atom.workspace.addModalPanel(item: @languageReasonView.getElement(), visible: false)

    # Events subscribed to in atom's system can be easily cleaned up with a CompositeDisposable
    @subscriptions = new CompositeDisposable

    # Register command that toggles this view
    @subscriptions.add atom.commands.add 'atom-workspace', 'language-reason:toggle': => @toggle()

  deactivate: ->
    @modalPanel.destroy()
    @subscriptions.dispose()
    @languageReasonView.destroy()

  serialize: ->
    languageReasonViewState: @languageReasonView.serialize()

  toggle: ->
    console.log 'LanguageReason was toggled!'

    if @modalPanel.isVisible()
      @modalPanel.hide()
    else
      @modalPanel.show()
