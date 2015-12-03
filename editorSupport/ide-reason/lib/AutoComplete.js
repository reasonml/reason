
/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the license found in the LICENSE file in
 * the root directory of this source tree.
 */

var _slicedToArray = (function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i['return']) _i['return'](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError('Invalid attempt to destructure non-iterable instance'); } }; })();

function _asyncToGenerator(fn) { return function () { var gen = fn.apply(this, arguments); return new Promise(function (resolve, reject) { var callNext = step.bind(null, 'next'); var callThrow = step.bind(null, 'throw'); function step(key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { Promise.resolve(value).then(callNext, callThrow); } } callNext(); }); }; }

var nuclideClient = require('nuclide-client');

var Reasonify = require('../Reasonify');
var getReasonifyConfig = require('./getReasonifyConfig');

var getServiceByNuclideUri = nuclideClient.getServiceByNuclideUri;

var child_process = require('child_process');
var descriptionPrefixRegex = /(let\s*\S*\s*:|type\s*\S*\s*=\s*)/;


/**
 * If we're attempting to format over this amount, it's probably trying to
 * format all pervasives. We shouldn't ever even get to that point.
 */
var MAX_ENTRIES_TO_TRY = 90;


/*
 * Merlin response format:
 *
 *  Module entries look like this:
 *  ----------------------------
 *  desc: ""
 *  info: ""
 *  kind: "Module"
 *  name: "ExampleMod"
 *
 *  Value entries look like this.
 *  ----------------------------
 *  desc: "val map : ('a -> 'b) -> 'a list -> 'b list"
 *  info: ""
 *  kind: "Value"
 *  name: "map"
 *
 *  Type entries look like this.
 *  ----------------------------
 *  desc: "type reexportedObscureType =↵ M_CommonMLExampleDependency__ExampleMod.reexportedObscureType"
 *  info: ""
 *  kind: "Type"
 *  name: "reexportedObscureType"
 */
var MerlinResponseFormatter = {
  signatureItemFormattingCache: {},
  needsLookup: function(outputEntries) {
    for (var i = 0; i < outputEntries.length; i++) {
      var desc = outputEntries[i].desc;
      if (desc !== '' && !MerlinResponseFormatter.signatureItemFormattingCache[desc]) {
        return true;
      }
    }
    return false;
  },
  trimReasonifiedDescription: function(desc) {
    // Conveniently this won't match types of the form: type t 'a = list 'a;
    var withoutPrefix = desc.replace(descriptionPrefixRegex, function(x) {return "";}).trim();
    var ret = (withoutPrefix[withoutPrefix.length - 1]) === ';' ?
      withoutPrefix.substr(0, withoutPrefix.length - 1) :
      withoutPrefix;
    return ret.trim();
  },
  /**
   * Normalizes Merlin response entries into formatted, pretty printed
   * names/descriptions.
   */
  normalizeCompletionItems: function(outputEntries, linePrefix) {
    var newReasonifiedCompletionDescriptions = null;
    var needsLookup = MerlinResponseFormatter.needsLookup(outputEntries);
    var definitelyOldMerlin = false;
    var definitelyNewMerlin = false;
    if (needsLookup) {
      var niceOutputEntriesWithFixedTypeConstructors = outputEntries.map(function(entry) {
        var definitelyNewMerlin =
          definitelyNewMerlin ||
          (entry.kind === 'Value' && !entry.desc.startsWith('val') && !entry.desc.startsWith('external')) ||
          (entry.kind === 'Constructor' && entry.desc.indexOf(entry.name + ' :') !== 0);
        var definitelyOldMerlin =
          definitelyOldMerlin ||
          !definitelyNewMerlin ||
          (entry.kind === 'Value' && entry.desc.startsWith('val') || entry.desc.startsWith('external')) ||
          (entry.kind === 'Constructor' && entry.desc.indexOf(entry.name + ' :') === 0);

        // Special case crashes formatter.
        if (entry.desc == "type 'a list = [] | :: of 'a * 'a list") {
          return "type 'a list = list";
        }
        if (entry.kind === 'Value') {
          if (definitelyNewMerlin) {
            // New merlin has no "val" prefix, insert type.
            return Reasonify.niceifyType('type t = ' + entry.desc);
          } else {
            // Else, old merlin already has a "val: or external:"
            return Reasonify.niceifyType(entry.desc);
          }
        }
        if (entry.kind === 'Constructor') {
          // Old merlin lists constructor descriptions as:
          //    Constructor : int * int
          if (definitelyOldMerlin) {
            return Reasonify.niceifyType('type t = | ' + entry.desc);
          } else {
            // Else, it's just contains a type description
            return Reasonify.niceifyType('type t = ' + entry.desc);
          }
        }
        return Reasonify.niceifyType(entry.desc);
      });
      try {
        var reasonifyConfig = getReasonifyConfig();
        Reasonify.setConfig(reasonifyConfig);

        // Sometimes there's an error because it's not as simple as creating a single mli file.
        var newReasonifiedCompletionDescriptions =
          !reasonifyConfig.pathToReasonfmt ? niceOutputEntriesWithFixedTypeConstructors :
          Reasonify.reasonifyManySignatureItems(niceOutputEntriesWithFixedTypeConstructors, 999);
      } catch (e) {
        // TODO: Log this to improve tooling. For now, we can just fall back to
        // using the cache on an individual result level.
        console.log('Could not format types for completion: This happens occasionally but report if you see it happen commonly (linePrefix:' + linePrefix + ')');
      }
    }
    var atomFormattedCompletions = outputEntries.map(function(entry, i) {
      var desc =
        entry.desc && newReasonifiedCompletionDescriptions && newReasonifiedCompletionDescriptions[i] ?
        MerlinResponseFormatter.trimReasonifiedDescription(newReasonifiedCompletionDescriptions[i]) :
        MerlinResponseFormatter.signatureItemFormattingCache[entry.desc];
      if (entry.desc != '' && !MerlinResponseFormatter.signatureItemFormattingCache[entry.desc]) {
        MerlinResponseFormatter.signatureItemFormattingCache[entry.desc] = desc;
      }
      return {
        desc: desc,
        info: entry.info,
        kind: entry.kind,
        name: entry.name
      };
    });
    return atomFormattedCompletions;
  }
};

module.exports = {
  getAutocompleteSuggestions: _asyncToGenerator(function* (request) {
    var editor = request.editor;
    var prefix = request.prefix;
    // OCaml.Pervasives has a lot of stuff that gets shown on every keystroke without this.
    if (prefix.trim().length === 0) {
      return [];
    }
    var path = editor.getPath();
    var merlinService = getServiceByNuclideUri('MerlinService', path);
    var text = editor.getText();
    var _editor$getCursorBufferPosition$toArray = editor.getCursorBufferPosition().toArray();
    var _editor$getCursorBufferPosition$toArray2 = _slicedToArray(_editor$getCursorBufferPosition$toArray, 2);
    var line = _editor$getCursorBufferPosition$toArray2[0];
    var col = _editor$getCursorBufferPosition$toArray2[1];

    // The default prefix at something like `Printf.[cursor]` is just the dot. Compute
    // `linePrefix` so that ocamlmerlin gets more context. Compute `replacementPrefix`
    // to make sure that the existing dot doesn't get clobbered when autocompleting.
    var linePrefix = editor.lineTextForBufferRow(line).substring(0, col);
    if (linePrefix.length > 0) {
      linePrefix = linePrefix.split(/([ \t\[\](){}<>,+*\/-])/).slice(-1)[0];
    }
    if (linePrefix.trim() == "") {
      return [];
    }
    var replacementPrefix = prefix;
    if (replacementPrefix.startsWith('.')) {
      replacementPrefix = replacementPrefix.substring(1);
    }

    yield merlinService.pushNewBuffer(path, text);
    var output = yield merlinService.complete(path, line, col, linePrefix);
    if (!output) {
      return null;
    }
    // Different versions of merlin might return different formats - normalize array.
    var outputEntries = 'length' in output ? output : output.entries;
    var normalizedCompletionItems = MerlinResponseFormatter.normalizeCompletionItems(outputEntries, linePrefix);
    /*
     * Type must be one of the following:
     * ----------------------------------
     * variable, constant, property, value, method, function, class, type,
     * keyword, tag, snippet, import, require
     */
    var merlinKindToAtomType = {
      'Type': 'type',
      'Value': 'value',
      'Module': 'require',
      'Constructor': 'class'
    };
    return normalizedCompletionItems.map(function (item) {
      var type = merlinKindToAtomType[item.kind] || 'value';
      return {
        text: item.name,
        type: type,
        description: item.desc,
        replacementPrefix: replacementPrefix
      };
    });
  })
};
