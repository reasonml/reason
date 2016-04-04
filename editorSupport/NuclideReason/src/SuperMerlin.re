/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
let startedMerlin: ref (option Js.Unsafe.any) = {contents: None};

/* This and the subsequent big js blocks are copied over from Nuclide. More convenient for now. */
let findNearestMerlinFile' = Js.Unsafe.js_expr {|
  function findNearestMerlinFile(beginAtFilePath) {
    var path = require('path');
    var fs = require('fs');
    var fileDir = path.dirname(beginAtFilePath);
    var currentPath = path.resolve(fileDir);
    do {
      var fileToFind = path.join(currentPath, '.merlin');
      var hasFile = fs.existsSync(fileToFind);
      if (hasFile) {
        return path.dirname(currentPath);
      }

      if (path.dirname(currentPath) === currentPath) {
        // Bail
        return '.';
      }
      currentPath = path.dirname(currentPath);
    } while (true);
  }
|};

let findNearestMerlinFile beginAtFilePath::path => {
  let result = Js.Unsafe.fun_call findNearestMerlinFile' [|Js.Unsafe.inject (Js.string path)|];
  Js.to_string result
};

let createMerlinProcessOnce' = Js.Unsafe.js_expr {|
  function createThingOnce(ocamlMerlinPath, ocamlMerlinFlags, dotMerlinDir) {
    var spawn = require('child_process').spawn;
    // To split while stripping out any leading/trailing space, we match on all
    // *non*-whitespace.
    var items = ocamlMerlinFlags.match(/\S+/g);
    var merlinProcess = spawn(ocamlMerlinPath, items, {cwd: dotMerlinDir});
    merlinProcess.stderr.on('data', function(d) {
      console.error('Ocamlmerlin: something wrong happened:');
      console.error(d.toString());
    });

    merlinProcess.stdout.on('close', function(d) {
      console.error('Ocamlmerlin: closed.');
    });

    return merlinProcess;
  }
|};

let createMerlinProcessOnce pathToMerlin::pathToMerlin merlinFlags::merlinFlags dotMerlinPath::dotMerlinPath =>
  Js.Unsafe.fun_call
    createMerlinProcessOnce'
    [|
      Js.Unsafe.inject (Js.string pathToMerlin),
      Js.Unsafe.inject (Js.string merlinFlags),
      Js.Unsafe.inject (Js.string dotMerlinPath)
    |];

let getMerlinProcess path::path =>
  switch startedMerlin.contents {
  | Some m => m
  | None => {
      let nuclideOCamlPathToMerlin = Atom.Config.get "nuclide.nuclide-ocaml.pathToMerlin";
      let nuclideOCamlMerlinFlags = Atom.Config.get "nuclide.nuclide-ocaml.merlinFlags";
      let nuclideOCamlPathToMerlinOverwrite = Atom.Config.get "nuclide.nuclide-ocaml.nuclideReasonOverwroteYour_pathToMerlin";
      let nuclideOCamlMerlinFlagsOverwrite = Atom.Config.get "nuclide.nuclide-ocaml.nuclideReasonOverwroteYour_merlinFlags";
      let nuclideReasonPathToMerlin = Atom.Config.get "NuclideReason.pathToMerlin";
      let nuclideReasonMerlinFlags = Atom.Config.get "NuclideReason.merlinFlags";
      let nuclideReasonMerlinLogFile = Atom.Config.get "NuclideReason.merlinLogFile";
      switch nuclideReasonMerlinLogFile {
      | JsonString "" => ()
      | JsonString s => Atom.Env.setEnvVar "MERLIN_LOG" s
      | _ => ()
      };
      switch (nuclideOCamlPathToMerlin, nuclideOCamlPathToMerlinOverwrite) {
      | (_, Empty) =>
          Atom.Config.set
            "nuclide.nuclide-ocaml.nuclideReasonOverwroteYour_pathToMerlin" nuclideOCamlPathToMerlin
      | _ => ()
      };
      switch (nuclideOCamlMerlinFlags, nuclideOCamlMerlinFlagsOverwrite) {
      | (_, Empty) =>
          Atom.Config.set
            "nuclide.nuclide-ocaml.nuclideReasonOverwroteYour_merlinFlags" nuclideOCamlMerlinFlags
      | _ => ()
      };
      Atom.Config.set "nuclide.nuclide-ocaml.pathToMerlin" nuclideReasonPathToMerlin;
      Atom.Config.set "nuclide.nuclide-ocaml.merlinFlags" nuclideReasonMerlinFlags;
      let merlinProcess =
        createMerlinProcessOnce
          pathToMerlin::(Atom.JsonValue.unsafeExtractString nuclideOCamlPathToMerlin)
          merlinFlags::(Atom.JsonValue.unsafeExtractString nuclideOCamlMerlinFlags)
          dotMerlinPath::(findNearestMerlinFile beginAtFilePath::path);
      startedMerlin.contents = Some merlinProcess;
      merlinProcess
    }
  };

let readOneLine merlinProcess::merlinProcess cmd::cmd => {
  let func = Js.Unsafe.js_expr {|
    function(merlinProcess, cmd) {
      return new Promise(function(resolve, reject) {
        var cmdString = JSON.stringify(cmd);
        var readline = require('readline');
        var reader = readline.createInterface({
          input: merlinProcess.stdout,
          terminal: false,
        });

        reader.on('line', function(line) {
          reader.close();
          var response;
          try {
            response = JSON.parse(line);
          } catch (err) {
            response = null;
          }
          if (!response || !Array.isArray(response) || response.length !== 2) {
            reject(Error('Unexpected ocamlmerlin output format: ' + line));
            return;
          }

          var status = response[0];
          var content = response[1];

          var errorResponses = {
            'failure': true,
            'error': true,
            'exception': true,
          };

          if (errorResponses[status]) {
            reject(Error('Ocamlmerlin returned an error: ' + line));
            return;
          }

          resolve(content);
        });

        merlinProcess.stdin.write(cmdString);
      })
    }
  |};
  /* TODO: arbitrary input returns nothing */
  Js.Unsafe.fun_call func [|Js.Unsafe.inject merlinProcess, Js.Unsafe.inject cmd|]
};

/* contextify is important for avoiding different buffers calling the backing merlin at the same time. */
/* https://github.com/the-lambda-church/merlin/blob/d98a08d318ca14d9c702bbd6eeadbb762d325ce7/doc/dev/PROTOCOL.md#contextual-commands */
let contextify query::query path::path => Js.Unsafe.obj [|
  ("query", Js.Unsafe.inject query),
  ("context", Js.Unsafe.inject (Js.array [|Js.string "auto", Js.string path|]))
|];

/* The tell command allows us to synchronize our text with Merlin's internal buffer. */
let makeTellCommand text => Js.array [|Js.string "tell", Js.string "start", Js.string "end", Js.string text|];

let positionToJsMerlinPosition (line, col) => Js.Unsafe.obj [|
  /* lines (rows) are 1-based for merlin, not 0-based, like for Atom */
  ("line", Js.Unsafe.inject (Js.number_of_float (float_of_int (line + 1)))),
  ("col", Js.Unsafe.inject (Js.number_of_float (float_of_int col)))
|];

let makeTypeHintCommand position => Js.array [|
  Js.Unsafe.inject (Js.string "type"),
  Js.Unsafe.inject (Js.string "enclosing"),
  Js.Unsafe.inject (Js.string "at"),
  Js.Unsafe.inject (positionToJsMerlinPosition position)
|];

let getTypeHint path::path text::text position::position => {
  let merlin = getMerlinProcess path;
  let res = readOneLine merlinProcess::merlin cmd::(contextify query::(makeTellCommand text) path::path);
  Js.Unsafe.meth_call
    res
    "then"
    [|
      Js.Unsafe.inject (
        Js.wrap_callback (
          fun _ =>
            readOneLine
              merlinProcess::merlin cmd::(contextify query::(makeTypeHintCommand position) path::path)
        )
      )
    |]
};

let makeCompleteCommand position prefix => Js.array [|
  Js.Unsafe.inject (Js.string "complete"),
  Js.Unsafe.inject (Js.string "prefix"),
  Js.Unsafe.inject (Js.string prefix),
  Js.Unsafe.inject (Js.string "at"),
  Js.Unsafe.inject (positionToJsMerlinPosition position)
|];

let getAutoCompleteSuggestions path::path text::text position::position prefix::prefix => {
  let merlin = getMerlinProcess path;
  let res = readOneLine merlinProcess::merlin cmd::(contextify query::(makeTellCommand text) path::path);
  Js.Unsafe.meth_call
    res
    "then"
    [|
      Js.Unsafe.inject (
        Js.wrap_callback (
          fun _ =>
            readOneLine
              merlinProcess::merlin cmd::(contextify query::(makeCompleteCommand position prefix) path::path)
        )
      )
    |]
};

let getDiagnostics path::path text::text => {
  let merlin = getMerlinProcess path;
  let res = readOneLine merlinProcess::merlin cmd::(contextify query::(makeTellCommand text) path::path);
  Js.Unsafe.meth_call
    res
    "then"
    [|
      Js.Unsafe.inject (

        Js.wrap_callback (
          fun _ =>
            readOneLine
              merlinProcess::merlin
              cmd::(contextify query::(Js.array [|Js.Unsafe.inject (Js.string "errors")|]) path::path)
        )
      )
    |]
};

let makeLocateCommand position extension => Js.array [|
  Js.Unsafe.inject (Js.string "locate"),
  Js.Unsafe.inject (Js.string ""),
  Js.Unsafe.inject (Js.string extension),
  Js.Unsafe.inject (Js.string "at"),
  Js.Unsafe.inject (positionToJsMerlinPosition position)
|];

/* TODO: put this logic into reason and somewhere else. */
let normalizeLocateCommandResult' = Js.Unsafe.js_expr {|
  function(o, path) {
    if (typeof o === "string") {
      return o
    }
    if (o.file == null) {
      return {
        file: path,
        pos: o.pos,
      };
    }
    return o;
  }
|};

let normalizeLocateCommandResult o path =>
  Js.Unsafe.fun_call normalizeLocateCommandResult' [|o, Js.Unsafe.inject (Js.string path)|];

let locate path::path text::text extension::extension position::position => {
  let merlin = getMerlinProcess path;
  let res = readOneLine merlinProcess::merlin cmd::(contextify query::(makeTellCommand text) path::path);
  let res1 =
    Js.Unsafe.meth_call
      res
      "then"
      [|
        Js.Unsafe.inject (
          Js.wrap_callback (
            fun _ =>
              readOneLine
                merlinProcess::merlin
                cmd::(contextify query::(makeLocateCommand position extension) path::path)
          )
        )
      |];
  Js.Unsafe.meth_call
    res1
    "then"
    [|Js.Unsafe.inject (Js.wrap_callback (fun result => normalizeLocateCommandResult result path))|]
};
