/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
/* Only temporarily needed. */
let jsTrimTrailing = Js.Unsafe.js_expr "function(s) {return s.split('\\n').map(function(ss) {return ss.trimRight()}).join('\\n');}";

let atomGlobal = Js.Unsafe.js_expr "atom";

let atomPoint = Js.Unsafe.js_expr "require('atom').Point";

let atomRange = Js.Unsafe.js_expr "require('atom').Range";

let bufferedProcess = Js.Unsafe.js_expr "require('atom').BufferedProcess";

/* Yes, this doesn't work across iframes. */
let _isString = Js.Unsafe.js_expr "function(s) {return typeof s === 'string' || s instanceof String;}";

let _isNumber = Js.Unsafe.js_expr "function(n) {return typeof n === 'number';}";

let _isUndefined = Js.Unsafe.js_expr "function(n) {return typeof n === 'undefined';}";

let _isNull = Js.Unsafe.js_expr "function(n) {return n === null;}";

let _isBool = Js.Unsafe.js_expr "function(n) {return typeof n === 'boolean';}";

let _arrayIsArray = Js.Unsafe.js_expr "Array.isArray";

let promise = Js.Unsafe.js_expr "Promise";

let emptyArgs = [||];

let trimTrailingWhiteSpace (s: string) => Js.to_string (
  Js.Unsafe.fun_call jsTrimTrailing [|Js.Unsafe.inject (Js.string s)|]
);

let module JsonType = {
  type t =
    | JsonString string
    | JsonNum float
    | JsonBool bool
    | JsonArray (array t)
    | JsonNull
    | Empty;
};

type completionEntry = {desc: string, info: string, kind: string, name: string};

/**
 * Api over Js values that are known to adhere to "Json" style constraints (no
 * functions), untyped, but predictable.
 */
module type JsonValueSig = {
  /* We never want to reveal that t is actually just an unsafe JS value.
   * We want everyone to go through the Apis which provide type safety. */
  type t;
  let fromJs: Js.t Js.js_string => JsonType.t;
  let toJs: JsonType.t => Js.Unsafe.any;
  let unsafeExtractString: JsonType.t => string;
};

let module JsonValue: JsonValueSig = {
  open JsonType;
  type t = Js.Unsafe.any;
  let rec fromJs fieldVal =>
    if (Js.Unsafe.fun_call _arrayIsArray [|Js.Unsafe.inject fieldVal|]) {
      let jsArray = Array.map fromJs (Js.to_array (Js.Unsafe.coerce fieldVal));
      JsonArray jsArray
    } else if (
      Js.Unsafe.fun_call _isString [|Js.Unsafe.inject fieldVal|]
    ) {
      JsonString (Js.to_string fieldVal)
    } else if (
      Js.Unsafe.fun_call _isBool [|Js.Unsafe.inject fieldVal|]
    ) {
      JsonBool (Js.to_bool (Js.Unsafe.coerce fieldVal))
    } else if (
      Js.Unsafe.fun_call _isNumber [|Js.Unsafe.inject fieldVal|]
    ) {
      JsonNum (Js.float_of_number (Js.Unsafe.coerce fieldVal))
    } else if (
      Js.Unsafe.fun_call _isNull [|Js.Unsafe.inject fieldVal|]
    ) {
      JsonNull
    } else {
      Empty
    };
  let rec toJs =
    fun
    | JsonString str => Js.Unsafe.inject (Js.string str)
    | JsonNum f => Js.Unsafe.inject (Js.float f)
    | JsonBool b => Js.Unsafe.inject (Js.bool b)
    | JsonArray a => Js.Unsafe.inject (Js.array (Array.map toJs a))
    | JsonNull => Js.Unsafe.inject Js.null
    | Empty => Js.Unsafe.inject Js.undefined;
  let unsafeExtractString o =>
    switch o {
    | JsonType.JsonString s => s
    | _ => raise (Invalid_argument "unsafeExtractString: not a string")
    };
};

let module Env = {
  let setEnvVar envVar strVal => {
    Js.Unsafe.set
      (Js.Unsafe.get (Js.Unsafe.get Js.Unsafe.global "process") "env") envVar (Js.string strVal);
  };
};

let module Config = {
  let get configKey :JsonType.t => {
    let config = Js.Unsafe.get atomGlobal "config";
    JsonValue.fromJs (Js.Unsafe.meth_call config "get" [|Js.Unsafe.inject (Js.string configKey)|])
  };
  let set configKey (v: JsonType.t) :unit => {
    let config = Js.Unsafe.get atomGlobal "config";
    let jsVal = JsonValue.toJs v;
    Js.Unsafe.meth_call
      config "set" [|Js.Unsafe.inject (Js.string configKey), Js.Unsafe.inject jsVal|]
  };
};

let module Point = {
  type t = (int, int);
  let toJs (row, column) =>
    Js.Unsafe.new_obj atomPoint [|Js.Unsafe.inject row, Js.Unsafe.inject column|];
  let fromJs jsP =>
    if (Js.Unsafe.fun_call _arrayIsArray [|Js.Unsafe.inject jsP|]) {
      let arr = Js.to_array jsP;
      (arr.(0), arr.(1))
    } else {
      let row = Js.Unsafe.get jsP "row";
      let col = Js.Unsafe.get jsP "column";
      (row, col)
    };
};

let module Range = {
  type row = int;
  type column = int;
  type rowColumn = (row, column);
  type t = (rowColumn as 'start, rowColumn as 'endd);
  let emptyRange = ((0, 0), (0, 0));
  let toJs (startRowColumn, endRowColumn) =>
    Js.Unsafe.new_obj atomRange [|Point.toJs startRowColumn, Point.toJs endRowColumn|];
  let fromJs jsRange =>
    if (Js.Unsafe.fun_call _arrayIsArray [|Js.Unsafe.inject jsRange|]) {
      let arr = Js.to_array jsRange;
      let startPoint = Js.to_array arr.(0);
      let endPoint = Js.to_array arr.(1);
      ((startPoint.(0), startPoint.(1)), (endPoint.(0), endPoint.(1)))
    } else {
      let startPoint = Js.Unsafe.get jsRange "start";
      let endPoint = Js.Unsafe.get jsRange "end";
      (Point.fromJs startPoint, Point.fromJs endPoint)
    };
};

let module Buffer = {
  type t = Js.Unsafe.any;
  let fromJs jsBuffer :t => jsBuffer;
  let toJs (buffer: t) :Js.Unsafe.any => buffer;
  let characterIndexForPosition (buffer: t) rowColumn =>
    Js.Unsafe.meth_call
      buffer "characterIndexForPosition" [|Js.Unsafe.inject (Point.toJs rowColumn)|];
  let getText (buffer: t) :string => Js.to_string (Js.Unsafe.meth_call buffer "getText" emptyArgs);
  let getTextInRange (buffer: t) (range: Range.t) :string => Js.to_string (
    Js.Unsafe.meth_call buffer "getTextInRange" [|Range.toJs range|]
  );
};

let module Cursor = {
  /* Actually just the underlying JS value - TODO: obscure this using
   * signature. */
  type t;
  let getBufferPosition (cursor: t) => Point.fromJs (
    Js.Unsafe.meth_call cursor "getBufferPosition" emptyArgs
  );
  let setBufferPosition position::(pos: Point.t) autoScroll::(autoScroll: bool) (cursor: t) => {
    let optionsObj = Js.Unsafe.obj [|("autoscroll", Js.Unsafe.inject (Js.bool autoScroll))|];
    let args = [|Point.toJs pos, optionsObj|];
    Point.fromJs (Js.Unsafe.meth_call cursor "setBufferPosition" args)
  };
  let fromJs jsCursor => jsCursor;
};

let module Grammar = {
  type t;
  let name grammar => Js.to_string (Js.Unsafe.get grammar "name");
};

let module Editor = {
  type t = Js.Unsafe.any;
  let fromJs jsEditor :t => jsEditor;
  let toJs (editor: t) :Js.Unsafe.any => editor;
  let lineTextForBufferRow (editor: t) bufferRow => Js.to_string (
    Js.Unsafe.meth_call editor "lineTextForBufferRow" [|bufferRow|]
  );
  let getBuffer editor :Buffer.t => Js.Unsafe.meth_call editor "getBuffer" emptyArgs;
  let getPath editor => {
    let path = Js.Unsafe.meth_call editor "getPath" emptyArgs;
    Js.Opt.test path ? Some (Js.to_string path) : None
  };
  let getCursors editor => {
    let arr =
      Array.map Cursor.fromJs (Js.to_array (Js.Unsafe.meth_call editor "getCursors" emptyArgs));
    Array.to_list arr
  };
  let getGrammar editor :Grammar.t => Js.Unsafe.meth_call editor "getGrammar" emptyArgs;
  let setSelectedBufferRanges editor bufferRanges => {
    let arr = Array.map Range.toJs bufferRanges |> Js.array;
    Js.Unsafe.meth_call editor "setSelectedBufferRanges" [|Js.Unsafe.inject arr|]
  };
};

/**
 * Dirt simple promise wrapper, not implementing advanced features yet.
 */
let module Promise = {
  /* This should be made abstract so it's essentially private. */
  type underlyingJsPromise;
  type t 'i 'o = {
    underlyingJsPromise: underlyingJsPromise,
    /* Try adding constraint where 'o = t 'ii 'oo for some 'ii 'oo */
    /* Perhaps the higher ranked polymorphism feature would work here */
    /* One promise's output is another promise's input. */
    thn: 'nextO .('i => 'o) => t 'o 'nextO
  };
  let toJs p => p.underlyingJsPromise;
  let create executor => {
    let jsCurriedExecutor = Js.wrap_callback executor;
    let underlyingJsPromise = Js.Unsafe.new_obj promise [|Js.Unsafe.inject jsCurriedExecutor|];
    {
      underlyingJsPromise,
      thn: fun onResolve =>
        Js.Unsafe.meth_call
          underlyingJsPromise "then" [|Js.Unsafe.inject (Js.wrap_callback onResolve)|]
    }
  };
  let resolve v => create (fun jsResolveCb jsRejectCb => Js.Unsafe.fun_call jsResolveCb [|v|]);
  /* For our current purposes, a promise (with no chaining abilities) is enough. */
  let createFakePromise (executor: ('a => unit) => ('b => unit) => unit) =>
    Js.Unsafe.new_obj promise [|Js.Unsafe.inject (Js.wrap_callback executor)|];
};

let module Notification = {
  type t;
  let getType (n: t) => Js.to_string (Js.Unsafe.meth_call n "getType" emptyArgs);
  let getMessage (n: t) => Js.to_string (Js.Unsafe.meth_call n "getMessage" emptyArgs);
  let dismiss (n: t) => Js.Unsafe.meth_call n "dismiss" emptyArgs;
};

let module NotificationManager = {
  type options = {detail: string, dismissable: bool, icon: string};
  let optionsToJs opts => Js.Unsafe.obj [|
    ("detail", Js.Unsafe.inject (Js.string opts.detail)),
    ("dismissable", Js.Unsafe.inject (Js.bool opts.dismissable)),
    ("icon", Js.Unsafe.inject (Js.string opts.icon))
  |];
  let defaultOptions = {detail: "MessageNotProvided", dismissable: false, icon: "flame"};
  let addError options::opts={...defaultOptions, icon: "flame"} title :unit =>
    Js.Unsafe.meth_call
      (Js.Unsafe.get atomGlobal "notifications")
      "addError"
      [|Js.Unsafe.inject (Js.string title), optionsToJs opts|];
  let addWarning options::opts={...defaultOptions, icon: "alert"} title :unit =>
    Js.Unsafe.meth_call
      (Js.Unsafe.get atomGlobal "notifications")
      "addWarning"
      [|Js.Unsafe.inject (Js.string title), optionsToJs opts|];
  let addInfo options::opts={...defaultOptions, icon: "info"} title :unit =>
    Js.Unsafe.meth_call
      (Js.Unsafe.get atomGlobal "notifications")
      "addInfo"
      [|Js.Unsafe.inject (Js.string title), optionsToJs opts|];
  let addSuccess options::opts={...defaultOptions, icon: "check"} title :unit =>
    Js.Unsafe.meth_call
      (Js.Unsafe.get atomGlobal "notifications")
      "addSuccess"
      [|Js.Unsafe.inject (Js.string title), optionsToJs opts|];
  let getNotifications () => Array.to_list (
    Js.to_array (
      Js.Unsafe.meth_call (Js.Unsafe.get atomGlobal "notifications") "getNotifications" emptyArgs
    )
  );
};

let module Process = {
  type options = {cwd: string, detached: bool};
  let defaultOptions = {cwd: ".", detached: false};
};

let module ChildProcess = {
  type t;
  let writeStdin (process: t) str :unit =>
    Js.Unsafe.meth_call
      (Js.Unsafe.get process "stdin") "write" [|Js.Unsafe.inject (Js.string str)|];
  let endStdin (process: t) => Js.Unsafe.meth_call (Js.Unsafe.get process "stdin") "end" emptyArgs;
};

module type BufferedProcessSig = {
  type t;
  let create:
    options::Process.options? =>
    stdout::(string => unit)? =>
    stderr::(string => unit)? =>
    exit::(int => unit)? =>
    args::list string =>
    string =>
    t;
};

let module BufferedProcess = {
  open Process;
  type t;
  let create options::opts=? stdout::stdOut=? stderr::stdErr=? exit::exit=? cmd args::args => {
    let fields = [|
      ("command", Js.Unsafe.inject (Js.string cmd)),
      ("args", Js.Unsafe.inject (Js.array (Array.map Js.string (Array.of_list args))))
    |];
    let fields =
      switch opts {
      | None => fields
      | Some opts =>
        let jsOptions = Js.Unsafe.obj [|
          ("cwd", Js.Unsafe.inject opts.cwd),
          ("detached", Js.Unsafe.inject (Js.bool opts.detached))
        |];
        Array.append fields [|("options", jsOptions)|]
      };
    let fields =
      switch stdOut {
      | None => fields
      | Some so =>
        let cb jsStr => so (Js.to_string jsStr);
        Array.append fields [|("stdout", Js.Unsafe.inject (Js.wrap_callback cb))|]
      };
    let fields =
      switch stdErr {
      | None => fields
      | Some si =>
        let cb jsStr => si (Js.to_string jsStr);
        Array.append fields [|("stdin", Js.Unsafe.inject (Js.wrap_callback cb))|]
      };
    let fields =
      switch exit {
      | None => fields
      | Some e =>
        let cb eCode => e (Js.to_float eCode);
        Array.append fields [|("exit", Js.Unsafe.inject (Js.wrap_callback cb))|]
      };
    Js.Unsafe.new_obj bufferedProcess [|Js.Unsafe.obj fields|]
  };
  let onWillThrowError buffProcess fn :unit => {
    let wrappedCb = Js.wrap_callback (
      fun jsErrHandle =>
        fn (Js.Unsafe.get jsErrHandle "error") (Js.Unsafe.get jsErrHandle "handle")
    );
    Js.Unsafe.meth_call buffProcess "onWillThrowError" [|Js.Unsafe.inject wrappedCb|]
  };
  let process (bufferedProcess: t) => Js.Unsafe.get bufferedProcess "process";
};
