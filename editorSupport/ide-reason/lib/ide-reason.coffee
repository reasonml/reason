getReasonifyConfig = require './getReasonifyConfig'
AutoComplete = require './AutoComplete'
{PluginManager} = require './plugin-manager'
{MainMenuLabel, getEventType} = require './utils'
{CompositeDisposable} = require 'atom'
Reasonify = require '../Reasonify'
{validateCompile, validateParse, prettifyFile} = require './binutils/toolChain'
UPI = require './upi'


reasonifyConfig = getReasonifyConfig()
Reasonify.setConfig(reasonifyConfig)

module.exports = IdeReason =
  pluginManager: null
  disposables: null
  menu: null

  config:
    pathToCompiler:
      type: "string"
      default: "ocamlc"
      description: "Path to compiler backend"
    pathToReasonfmt:
      type: "string"
      default: "reasonfmt"
      description: "Path to Reason formatting program"
    printWidth:
      type: "integer"
      default: 110
      description: "Print width to wrap Reason lines at"
    onSavePrettify:
      type: "boolean"
      default: false
      description: "(NOT TESTED/WORKING) Run file through reasonfmt before save"

    switchTabOnCheck:
      type: "boolean"
      default: true
      description: "(NOT TESTED/WORKING) Switch to error tab after file check finished"
    expressionTypeInterval:
      type: "integer"
      default: 300
      description: "(NOT TESTED/WORKING) Type/Info tooltip show delay, in ms"
    onCursorMove:
      type: 'string'
      description: '''
      Show check results (error, lint) description tooltips
      when text cursor is near marker, close open tooltips, or do
      nothing?
      '''
      enum: ['Show Tooltip', 'Hide Tooltip', 'Nothing']
      default: 'Nothing'
    startupMessageIdeBackend:
      type: "boolean"
      default: true
      description: "(NOT TESTED/WORKING) Show info message about reason-ide-backend service on
                    activation"
    panelPosition:
      type: 'string'
      default: 'bottom'
      description: '''
      Output panel position
      '''
      enum: ['bottom', 'left', 'top', 'right']

  # TODO: Remove all the ghc related work here.
  cleanConfig: ->
    [ 'onSaveCheck'
    , 'onSaveLint'
    , 'onMouseHoverShow'
    , 'useLinter'
    ].forEach (item) ->
      if atom.config.get("ide-reason.#{item}")?
        atom.config.set "haskell-ghc-mod.#{item}", atom.config.get "ide-reason.#{item}"
      atom.config.unset "ide-reason.#{item}"

    if atom.config.get 'ide-reason.closeTooltipsOnCursorMove'
      atom.config.set 'ide-reason.onCursorMove', 'Hide Tooltip'

    [ 'useBackend'
    , 'useBuildBackend'
    , 'closeTooltipsOnCursorMove'
    ].forEach (item) ->
      atom.config.unset "ide-reason.#{item}"

    setTimeout (->
      newconf = {}

      serialize = (obj, indent = "") ->
        (for k, v of obj
          if typeof(v) is 'object'
            """
            #{indent}'#{k.replace /'/g, '\\\''}':
            #{serialize(v, indent+'  ')}
            """
          else
            """
            #{indent}'#{k.replace /'/g, '\\\''}': '#{v.replace /'/g, '\\\''}'
            """).join '\n'


      [ 'check-file'
      , 'lint-file'
      , 'show-type'
      , 'show-info'
      , 'show-info-fallback-to-type'
      , 'insert-type'
      , 'insert-import'
      ].forEach (item) ->
        kbs = atom.keymaps.findKeyBindings command: "ide-reason:#{item}"
        kbs.forEach ({selector, keystrokes}) ->
          newconf[selector] ?= {}
          newconf[selector][keystrokes] = "haskell-ghc-mod:#{item}"

      [ 'build'
      , 'clean'
      , 'test'
      , 'set-build-target'
      ].forEach (item) ->
        kbs = atom.keymaps.findKeyBindings command: "ide-reason:#{item}"
        kbs.forEach ({selector, keystrokes}) ->
          newconf[selector] ?= {}
          newconf[selector][keystrokes] = "ide-reason-cabal:#{item}"

      cs = serialize(newconf)
      if cs
        atom.workspace.open('ide-reason-keymap.cson').then (editor) ->
          editor.setText """
          # This is ide-reason system message
          # Most keybinding commands have been moved to backend packages
          # Please add the following to your keymap
          # in order to preserve existing keybindings.
          # This message won't be shown once there are no obsolete keybindings
          # anymore
          #{cs}
          """
      ), 1000

  activate: (state) ->
    @cleanConfig()
    @upiProvided = false

    if atom.config.get 'ide-reason.startupMessageIdeBackend'
      setTimeout (=>
        unless @upiProvided
          atom.notifications.addWarning """
          ide-reason needs backends that provide most of functionality.
          Please refer to README for details
          """,
          dismissable: true
        ), 5000

    @disposables = new CompositeDisposable

    @pluginManager = new PluginManager state

    # global commands
    @disposables.add atom.commands.add 'atom-workspace',
      'ide-reason:toggle-output': =>
        @pluginManager.togglePanel()

    atom.keymaps.add 'ide-reason',
      'atom-text-editor[data-grammar~="reason"]':
        'escape': 'ide-reason:close-tooltip'

    @menu = new CompositeDisposable
    @menu.add atom.menu.add [
      label: MainMenuLabel
      submenu : [
        {label: 'Prettify', command: 'ide-reason:prettify-file'}
        {label: 'Toggle Panel', command: 'ide-reason:toggle-output'}
      ]
    ]

  createAutocompleteProvider: (createAutocompleteProvider) ->
    selector: '.source.reason',
    inclusionPriority: 1,
    disableForSelector: '.source.reason .comment',
    getSuggestions: AutoComplete.getAutocompleteSuggestions


  getHyperclickProvider:() ->
    require('./HyperclickProvider')

  deactivate: ->
    @pluginManager.deactivate()
    @pluginManager = null

    atom.keymaps.removeBindingsFromSource 'ide-reason'

    # clear commands
    @disposables.dispose()
    @disposables = null

    @menu.dispose()
    @menu = null
    atom.menu.update()

  serialize: ->
    @pluginManager?.serialize()

  provideUpi: ->
    @upiProvided = true
    new UPI(@pluginManager)
    
  # Consumes its own provided service (so that it can be refactored later)
  consumeUPI: (service) ->
    upi = service.registerPlugin @disposables = new CompositeDisposable

    upi.setMessageTypes
      error: {}
      warning: {}
      lint: {}

    @disposables.add \
      atom.commands.add 'atom-text-editor[data-grammar~="reason"]',
        'ide-reason:prettify-file': ({target}) ->
          prettifyFile target.getModel(), upi
        'ide-reason:close-tooltip': ({target, abortKeyBinding}) =>
          if @pluginManager.controller(target.getModel()).hasTooltips()
            @pluginManager.controller(target.getModel()).hideTooltip()
          else
            abortKeyBinding?()
        'ide-reason:next-error': ({target}) =>
          @pluginManager.nextError()
        'ide-reason:prev-error': ({target}) =>
          @pluginManager.prevError()

    @disposables.add upi.onDidStopChanging (editor) ->
      if !@savedFile
        upi.clearMessages ['error', 'warning']
      @savedFile = false
    @disposables.add upi.onDidSaveBuffer (editor) ->
      @savedFile = true
      validateCompile editor, upi

    # @disposables.add upi.onDidSaveBuffer (buffer) ->
    #   checkLint buffer, 'Save'

      
