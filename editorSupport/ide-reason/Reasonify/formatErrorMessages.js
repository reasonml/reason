var clc = require('cli-color');
var jsdiff = require('diff');
var child_process = require('child_process');
var path = require('path');
var Reasonify = require('../Reasonify');
var desiredTableMargin = 4;
// Add one for the center line
var repeat  = function(ch, times) {
  var str = '';
  for (var i = 0; i < times; i++) {
    str += ch;
  }
  return str;
};

var renderFileRange = function(range) {
  return ":" + ("" + range[0][0]) + (
    range[0][1] != null ?
      " " + range[0][1] + (range[1] ? "-" + range[1][1] : "") :
      ""
  );
};

var renderClickableFileName = function(filePath, range) {
  var append = range ? renderFileRange(range) : "";
  return filePath + append;
};

var log = function() {
  console.log.apply(console.log, arguments);
};

var logError = function(msg) {
  console.log(clc.red(msg));
};
var logTitle = function() {
  var msg = clc.xterm(222);
  console.log(msg.apply(msg, arguments));
};
var logProgress = function() {
  var msg = clc.xterm(71);
  console.log(msg.apply(msg, arguments));
};

var error = function(txt) {
  return clc.red(txt);
};
var errorUnderline = function(txt) {
  return clc.red.underline(txt);
};
var errorUnderlineBold = function(txt) {
  return clc.red.bold.underline(txt);
};

var warn = function(txt) {
  return clc.cyan(txt);
};
var warnUnderline = function(txt) {
  return clc.cyan.underline(txt);
};
var warnUnderlineBold = function(txt) {
  return clc.cyan.bold.underline(txt);
};

var success = function(txt) {
  return clc.green(txt);
};
var successUnderline = function(txt) {
  return clc.green.underline(txt);
};
var successUnderlineBold = function(txt) {
  return clc.green.bold.underline(txt);
};

var lineNumber = function(txt) {
  return clc.blue(txt);
};

var titleBar = function(txt) {
  return txt;
};

var characterLength = function(txt) {
  return clc.getStrippedLength(txt);
};

var bold = function(txt) {
  return clc.bold(txt);
};

var actionTitle = function(txt) {
  return clc.yellow(txt);
};
var actionExample = function(txt) {
  return clc.white(txt);
};

var renderFileHeader = function (fileDiagnostic) {
  var fileDesc = renderClickableFileName(fileDiagnostic.filePath, fileDiagnostic.range);
  return [
    '',
    '',
    fileDiagnostic.type === 'ERROR' ?
      errorUnderlineBold('☒ ' + fileDesc) :
      warnUnderlineBold('☐ ' + fileDesc)
  ].join('\n');
};


var addLeftPadding = function(txt, amount) {
  var lines = txt.split('\n');
  return lines.map(function(line) {
    return repeat(' ', amount) + line;
  }).join('\n');
};

var longestLine = function(lines) {
  var max = 0;
  for (var i = 0; i < lines.length; i++) {
    max = Math.max(lines[i].length, max);
  }
  return max;
};

// I believe lines are base 1, characters are base zero.
var renderSourcePreview = function (diagnosticType, previewExpand, fileText, range) {
  if (!range || range[0] == null || range[1] == null) {
    return "";
  }
  var line = range[0][0];
  var characterStart = range[0][1];
  var characterEnd = range[1][1];
  if (previewExpand == -1) {
    return "";
  } else {
    var lines = fileText.split('\n');
    var previewLines = [];
    var indexOfLine = line - 1;
    var previewBackwards = indexOfLine - Math.max(indexOfLine - previewExpand, 0);
    var previewForwards = Math.min(indexOfLine + previewExpand, lines.length) - indexOfLine;
    var start = indexOfLine - previewBackwards;
    var stop = indexOfLine + previewForwards;
    var indent = repeat(' ', Reasonify.config.indentCode);
    var slicedLines = lines.slice(start, start + previewBackwards + previewForwards - 1);
    var ret = Reasonify.removeLeftPadding(slicedLines);
    var lineNoSpaces = ("" + stop).length + 4; // One for dot, three for spaces
    if (start !== 0) {
      previewLines.push(indent + repeat(' ', lineNoSpaces) + "…")
    }
    var adjustedCharacterStart = characterStart - ret.removedLeftPadding;
    var adjustedCharacterEnd = characterEnd - ret.removedLeftPadding;
    for (var i = 0; i < ret.lines.length; i++) {
      var lineToShow = ret.lines[i];
      var lineNo = lineNumber((i + start + 1) + '.   ');
      var sourceLine =
        i != previewBackwards ? lineToShow :
        lineToShow.slice(0, adjustedCharacterStart) +
        (diagnosticType === 'ERROR' ? errorUnderline : warnUnderline)
        (lineToShow.slice(adjustedCharacterStart, adjustedCharacterEnd + 1)) +
        // (lineToShow.slice(adjustedCharacterStart, adjustedCharacterEnd + 1)) +
        lineToShow.slice(adjustedCharacterEnd + 1);
      previewLines.push(indent + lineNo + sourceLine);
      // if (i == previewBackwards) {
      //   previewLines.push(
      //     indent +
      //     repeat(' ', lineNoSpaces + adjustedCharacterStart) +
      //     clc.red(repeat('~', adjustedCharacterEnd - adjustedCharacterStart)));
      // }
    }
    if (stop !== lines.length-1) {
      previewLines.push(indent + repeat(' ', lineNoSpaces) + "…")
    }
    previewLines.push('');
    return previewLines.join('\n');
  }
};


var addedDiffPartsByLine = function(parts) {
  var byLine = [[]];
  var lineNum = 0;
  var partInLine = -1;
  for (var i = 0; i < parts.length; i++) {
    var part = parts[i];
    if (part.removed) {
      continue;
    }
    partInLine++;
    byLine[lineNum][partInLine] = {value: '', added: part.added, removed:part.removed};
    for (var ch = 0; ch < part.value.length; ch++) {
      var character = part.value.charAt(ch);
      if (character == '\n') {
        lineNum++;
        byLine[lineNum] = [];
        partInLine = 0;
        byLine[lineNum][partInLine] = {value: '', added: part.added, removed:part.removed};
      } else {
        byLine[lineNum][partInLine].value += character;
      }
    }
  }
  return byLine;
};
var getDiffExpectedString = function(diffParts) {
  var str = "";
  diffParts.forEach(function(part) {
    if (part.added) {
      str += error(part.value);
    } else if (!part.removed) {
      str += part.value;
    }
  });
  return str;
};
var getDiffInferredString = function(diffParts) {
  var str = "";
  diffParts.forEach(function(part) {
    if (part.added) {
      str += success(part.value);
    } else if (!part.removed) {
      str += part.value;
    }
  });
  return str;
};

var renderTableLine = function(tableColumnWidth, margin) {
  var halfTitleBar = repeat('-', tableColumnWidth);
  var titleBarText = repeat(' ', margin) + halfTitleBar + "+" + halfTitleBar;
  return (titleBar(titleBarText));
};
var renderTable = function(tableColumnWidth, margin, leftTitle, rightTitle, inferredDiff, expectedDiff) {
  var lines = [];
  var marginSpace = repeat(' ', margin);
  var title =
    marginSpace +
    leftTitle + repeat(' ', tableColumnWidth - leftTitle.length) +
    "| " +
    rightTitle + repeat(' ', tableColumnWidth - rightTitle.length);
  lines.push(titleBar(title));
  lines.push(renderTableLine(tableColumnWidth, margin));
  // Have to split up all the parts more granularly based on line number
  var leftDiffPartsByLine = addedDiffPartsByLine(inferredDiff);
  var rightDiffPartsByLine = addedDiffPartsByLine(expectedDiff);
  var larger = Math.max(leftDiffPartsByLine.length, rightDiffPartsByLine.length);
  for (var i = 0; i < larger; i++) {
    var leftDiffString = leftDiffPartsByLine[i] ? getDiffExpectedString(leftDiffPartsByLine[i]) : "";
    var rightDiffString = rightDiffPartsByLine[i] ? getDiffInferredString(rightDiffPartsByLine[i]) : "";
    var leftHalf = leftDiffString +
      repeat(' ', tableColumnWidth - characterLength(leftDiffString));
    var rightHalf = rightDiffString;
    var line = marginSpace + leftHalf + titleBar("| ") + rightHalf;
    lines.push(line);
  }
  return lines.join('\n');
};


var reasonifyTypeLines = function(mlType, width) {
  try {
    return Reasonify.reasonifyTypeLines(mlType, width);
  } catch (e) {
    console.log(mlType, e);
    return "Cannot format/print type. Maybe your CommonML isn't up to date.\n" +
           "Otherwise, the extractor/formatter needs to be improved. Look at\n"+
           "the original error logs to diagnose your type error and report your\n" +
           "entire log output so we can fix this.";
  }
};

var reasonifyType = function(mlType, width) {
  return reasonifyTypeLines(mlType, width).join('\n').trimRight();
};


// For reasonifying a signature value.
var reasonifySignatureItemLines = function(mlType, width) {
  try {
    var processedItems = Reasonify.reasonifyManySignatureItems([mlType], width);
    var ret = (!processedItems || processedItems.length !== 1) ?
    console.log(mlType, e) &&
      "Cannot format/print type. Maybe your CommonML isn't up to date.\n" +
       "Otherwise, the extractor/formatter needs to be improved. Look at\n"+
       "the original error logs to diagnose your type error and report your\n" +
       "entire log output so we can fix this." :
       processedItems[0];
    return ret.split('\n');
  } catch (e) {
    throw e;
  }
};

var humanMessage = function(str) {
  return repeat(' ', Reasonify.config.indentHuman) + bold("> " + str);
};
var humanMessageNoArrow = function(str) {
  return repeat(' ', Reasonify.config.indentHuman) + bold("  " + str);
};

var codeMessage = function(str) {
  return str.split('\n').map(function(s) {
    return repeat(' ', Reasonify.config.indentCode) + s;
  }).join('\n');
};

// Source code example aware wrapping.
// http://stackoverflow.com/questions/14484787/wrap-text-in-javascript
function stringWrapper(str, width, spaceReplacer) {
  if (str.length>width) {
    var p=width
    for (;p>0 && str[p]!=' ';p--) {
    }
    if (p>0) {
      var left = str.substring(0, p);
      var right = str.substring(p+1);
      return left + spaceReplacer + stringWrapper(right, width, spaceReplacer);
    }
  }
  return str;
};
var helpPadding = 3;
var renderHelpSection = function(resolution) {
  var wrapTo = Math.round(4 * Reasonify.config.columns / 5);
  var lines = resolution.intro ? [stringWrapper(resolution.intro, wrapTo, '\n'), ''] : [];
  var i = 0;
  resolution.help.forEach(function(helpItem) {
    i++;
    var wrappedTitle = addLeftPadding(
      (resolution.help.length > 1 ? (i) + '. ' : '') +
      stringWrapper(helpItem.title, wrapTo, '\n'),
      helpPadding
    );
    lines.push(actionTitle(wrappedTitle));
    if (helpItem.example) {
      var wrappedExample =
        addLeftPadding(
          stringWrapper(helpItem.example,  wrapTo , '\n'),
          helpPadding * 2
        )
      lines.push(actionExample(wrappedExample));
    }
  });
  return lines.join('\n');
};

function lowerBase(name) {
  return name[0].toLowerCase() + name.substr(1);
}
function makeLines(n) {
  var arr = [];
  for (var i = 0; i < n; i++) {
    arr.push('');
  }
  return arr;
};

var joinArrays = function(arrays, joinWith) {
  var ret = [];
  for (var i = 0; i < arrays.length; i++) {
    var arr = arrays[i];
    ret = ret.concat(arr);
    if (i != arrays.length - 1) {
      ret.push(joinWith);
    }
  }
  return ret;
};

var renderDiffTableLines = function(inferredLines, expectedLines) {
  var inferred = inferredLines.join('\n');
  var expected = expectedLines.join('\n');
  var lines = [];

  // These are hard limits
  var cellPadding = 1;
  var minColumnWidth = cellPadding + Math.max(longestLine(inferredLines), longestLine(expectedLines));
  var centerLine = 2; // |<space>
  var maxColumnWidth = Reasonify.config.columns / 2 - centerLine;
  var amountOfMarginToPlayWith = Math.max(0, (maxColumnWidth - minColumnWidth));
  var margin = Math.min(amountOfMarginToPlayWith, desiredTableMargin);
  var expectedDiff = jsdiff.diffWords(inferred, expected);
  var inferredDiff = jsdiff.diffWords(expected, inferred);
  var lines = [];
  lines.push('');
  var tableColumnWidth = Math.min(minColumnWidth, maxColumnWidth);
  lines.push(
    renderTable (tableColumnWidth, margin, "Inferred", "Expected", inferredDiff, expectedDiff)
  );
  return lines;
};


var KindPrinters = {
  'General.CatchAll': function(diagnostic) {
    var msg = diagnostic.commonMLData.details.msg;
    return [
     "This error type has not been added to the error printer/extractor.",
      "Make sure everything you have is up to date. If the problem does not go",
      "away after updating, you can help fix this by adding the error ",
      "extractor/printer:",
      bold('Here\'s the raw, ugly error message in the mean time:'),
      error(msg)
    ].join('\n');
  },
  'FileErrors.SyntaxError': function(diagnostic) {
    var msg = diagnostic.commonMLData.details.msg;
    return humanMessage("Syntax error:" + diagnostic.commonMLData.details.hint);
  },
  "BuildErrors.InconsistentAssumptions": function(diagnostic) {
    var lines = [];
    var mainMessage =
      "These two files were compiled with different versions of " + diagnostic.commonMLData.details.moduleName + "'s interface: ";
    lines.push(humanMessage(mainMessage));
    lines.push(humanMessage(diagnostic.commonMLData.details.conflictOne));
    lines.push(humanMessage(diagnostic.commonMLData.details.conflictTwo));
    lines.push('');
    var resolution = {
      intro: "Here are some things you can try:",
      help: [
        {
          title: "This could be a problem with the build system (try cleaning, then rebuilding):",
          example: "There may be a bug that occurs when a module doesn't define an interface " +
            "file and one is automatically generated for it " +
            "when compiling the implementation. Then, later when you recompile incrementally, " +
            "nothing cleans up the old interface that was generated. You can help by fixing the build system!"
        },
        {
          title: "You might have compiled those two files against two incompatible versions of " +
                  diagnostic.moduleName + ":",
          example:
            "If this is the case, it could be that you're sharing/reusing cached build artifacts " +
            "(good for you in that case!) but not correctly deciding when to invalidate the cache."
        }
      ]
    };
    lines.push(renderHelpSection(resolution));
    return lines.join('\n');
  },
  'TypeErrors.SignatureItemMissing': function(diagnostic) {
    var lines = [];
    lines.push(humanMessage('This module doesn\'t inclue all of the required items:'));
    lines = lines.concat(diagnostic.commonMLData.details.missingItems.map(function(missingMsg) {
      return humanMessageNoArrow(error(missingMsg.trim()));
    }));
    return lines.join('\n');
  },
  'TypeErrors.SignatureMismatch': function(diagnostic) {
    var lines = [];
    var reInferredLines = [];
    var reExpectedLines = [];
    var niceInferredSignatureItem =
      diagnostic.commonMLData.details.inferredValueType && Reasonify.niceifyType(diagnostic.commonMLData.details.inferredValueType);
    var niceExpectedSignatureItem =
      diagnostic.commonMLData.details.expectedValueType && Reasonify.niceifyType(diagnostic.commonMLData.details.expectedValueType);
    var wrapAtColumnWidth = Math.round(Reasonify.config.columns / 2) - 4;
    var reInferredLines = reasonifySignatureItemLines(niceInferredSignatureItem, wrapAtColumnWidth);
    var reExpectedLines = reasonifySignatureItemLines(niceExpectedSignatureItem, wrapAtColumnWidth);

    var fileDesc = renderClickableFileName(diagnostic.commonMLData.details.declarationLocation.filePath, diagnostic.commonMLData.details.declarationLocation.range);
    lines.push(humanMessage(errorUnderlineBold(fileDesc)));
    var sourcePreview =
      diagnostic.commonMLData.details.declarationLocation.fileText ?
      renderSourcePreview(
        'ERROR',
        Reasonify.config.errorPreviewExpand,
        diagnostic.commonMLData.details.declarationLocation.fileText,
        diagnostic.commonMLData.details.declarationLocation.range
      ) :
      'No File Text To Preview';
    if (Reasonify.config.showSourcePreviews) {
      lines.push(sourcePreview);
    }
    lines.push(humanMessage('The type of the module value ' + error('here') + ' does not match what is ' + success('expected') + ':'));
    lines = lines.concat(renderDiffTableLines(reInferredLines, reExpectedLines));
    return lines.join('\n');
  },
  // "Equivalent types" are when the compiler emits errors in form of:
  //
  // type x = y is not compatible with type z = s
  'TypeErrors.IncompatibleType': function(diagnostic) {
    var lines = [];
    var reInferredLines = [];
    var reExpectedLines = [];
    var conflictPairs = diagnostic.commonMLData.details.conflicts;
    var termKind = diagnostic.commonMLData.details.termKind; // Either pattern or expresion.
    var inferredEquivalentTypes =
      diagnostic.commonMLData.details.inferredEquivalentTypes ||
      [diagnostic.commonMLData.details.inferred];
    var expectedEquivalentTypes =
      diagnostic.commonMLData.details.expectedEquivalentTypes ||
      [diagnostic.commonMLData.details.expected];
    for (var i = 0; i < Math.max(inferredEquivalentTypes.length, expectedEquivalentTypes.length); i++) {
      var inferredEquivalentType = inferredEquivalentTypes[i];
      var expectedEquivalentType = expectedEquivalentTypes[i];
      var niceInferredEquivalentType = inferredEquivalentType && Reasonify.niceifyType(inferredEquivalentType);
      var niceExpectedEquivalentType = expectedEquivalentType && Reasonify.niceifyType(expectedEquivalentType);
      var addedInferredLines = 
        niceInferredEquivalentType ?
          reasonifyTypeLines(niceInferredEquivalentType, Math.round(Reasonify.config.columns / 2) - 4) : [];
      var addedExpectedLines =
        niceExpectedEquivalentType ?
          reasonifyTypeLines(niceExpectedEquivalentType, Math.round(Reasonify.config.columns / 2) - 4) : [];

      if (i == 0 ||
         (niceInferredEquivalentType &&
          niceInferredEquivalentType.trim() !== Reasonify.niceifyType(inferredEquivalentTypes[0].trim()) ||
          niceExpectedEquivalentType &&
          niceExpectedEquivalentType.trim() !== Reasonify.niceifyType(expectedEquivalentTypes[0]).trim())) {
        if (i > 0) {
          reInferredLines = reInferredLines.concat(
              ['', 'Expanded Definition:']
          );
          reExpectedLines = reExpectedLines.concat(
              ['', 'Expanded Definition:']
          );
        }
        reInferredLines = reInferredLines.concat(addedInferredLines);
        reExpectedLines = reExpectedLines.concat(addedExpectedLines);
        reInferredLines = reInferredLines.concat(makeLines(Math.max(0, addedExpectedLines.length - addedInferredLines.length)));
        reExpectedLines = reExpectedLines.concat(makeLines(Math.max(0, addedInferredLines.length - addedExpectedLines.length)));
      }
    }
    for (var i = 0; i < conflictPairs.length; i++) {
      var inferredConflictEquivalentTypes = conflictPairs[i].inferred;
      var expectedConflictEquivalentTypes = conflictPairs[i].expected;
      reInferredLines = reInferredLines.concat(
          ['', 'Conflicting Portion:', '--------------------']
      );
      reExpectedLines = reExpectedLines.concat(
          ['', 'Conflicting Portion:', '--------------------']
      );
      var addedInferredLines = inferredConflictEquivalentTypes ?
        joinArrays(
          Reasonify.reasonifyEquivalentTypeLines(
            Reasonify.niceifyTypes(inferredConflictEquivalentTypes), Math.round(Reasonify.config.columns / 2) - 4),
          '  ='
        ) : [];
      var addedExpectedLines = 
        expectedConflictEquivalentTypes ?
          joinArrays(
            Reasonify.reasonifyEquivalentTypeLines(
              Reasonify.niceifyTypes(expectedConflictEquivalentTypes), Math.round(Reasonify.config.columns / 2) - 4
            ),
            '  ='
          ) : [];
      reInferredLines = reInferredLines.concat(addedInferredLines);
      reExpectedLines = reExpectedLines.concat(addedExpectedLines);
      reInferredLines = reInferredLines.concat(makeLines(Math.max(0, addedExpectedLines.length - addedInferredLines.length)));
      reExpectedLines = reExpectedLines.concat(makeLines(Math.max(0, addedInferredLines.length - addedExpectedLines.length)));
    }

    var mainMessage =
      "The type of " + errorUnderline("this " + termKind) + " doesn't match the " + success("expected") + " type";
    lines.push(humanMessage(mainMessage));

    lines = lines.concat(renderDiffTableLines(reInferredLines, reExpectedLines));
    if (diagnostic.commonMLData.details.existentialMessage) {
      lines.push('');
      var niceExistential = Reasonify.niceifyType(diagnostic.commonMLData.details.existentialMessage);
      lines.push(humanMessage(niceExistential));
    }
    return lines.join('\n');
  },
  'Warnings.CatchAll': function(diagnostic) {
    var lines = [];
    var flag = diagnostic.commonMLData.details.warningFlag;
    var resolution = {
      intro: null,
      help: [
        {
          title: '',
          example: "You may disable this warning by adding `-w -" +
            flag + "` to your compiler commands. In your package.json, " +
            "set CommonML.compileCommands to [\"-w -" + flag + "\"]"
        }
      ]
    };
    lines.push(humanMessage(diagnostic.commonMLData.details.warningMessage));
    lines.push(renderHelpSection(resolution));
    lines.push('');
    return lines.join('\n');
  },
  'TypeErrors.MismatchTypeArguments': function(diagnostic) {
    var typeConstructor = diagnostic.commonMLData.details.typeConstructor;
    var observedCount = diagnostic.commonMLData.details.observedCount;
    var expectedCount = diagnostic.commonMLData.details.expectedCount;
    var lines = [];
    if (observedCount < expectedCount) {
      lines.push(
        humanMessage("You have supplied " + error(observedCount) + " type arguments to " + Reasonify.niceifyType(typeConstructor) + " - but it expects " + success(expectedCount))
      );
    } else {
      lines.push(
        humanMessage("You have supplied " + error(observedCount) + " type arguments to " + Reasonify.niceifyType(typeConstructor) + " - but it only expects " + success(expectedCount))
      );
    }
    var resolution = {
      intro:null,
      help: [
        {
          title: "Learn More About Type Arguments:",
          example:
            "Types are very different from functions and values because types are purely a " +
            "compile-time concept - whereas functions and values are a run-time " +
            "concept. " +
            "However, types are similar to functions in that can " +
            "accept arguments. For example, \"list\" requires that " +
            "you supply it one argument such as \"list int\" - which is like " +
            "saying List<Int> in other languages. The syntax for passing type " +
            "arguments is the same syntax that is used " +
            "to pass function arguments - simply pass the arguments via a " +
            "space separated list. You may use parenthesis to convey precedence. For example " +
            "list (list int) is a list of (lists that contain integers)."
        }
      ]
    };
    lines.push('');
    lines.push(renderHelpSection(resolution));
    lines.push('');
    return lines.join('\n');
  },
  'TypeErrors.NotAFunction': function(diagnostic) {
    var type = diagnostic.commonMLData.details.type;
    var lines = [];
    lines.push(humanMessage("You are calling this as if it were a function, but it's not a function, it has type:"));
    var reType = reasonifyType(Reasonify.niceifyType(type), Math.round(Reasonify.config.columns / 2));
    lines.push('');
    lines.push(codeMessage(error(reType)));
    var resolution = {
      intro: null,
      help: [
        {
          title: "Did you forget a semicolon or comma?",
          example:
            "If you forget to add a semicolon or comma between two words, it will look like you're trying to call a function."
        }
      ]
    };
    lines.push('');
    lines.push('');
    lines.push(renderHelpSection(resolution));
    lines.push('');
    return lines.join('\n');
  },
  'TypeErrors.AppliedTooMany': function(diagnostic) {
    var type = diagnostic.commonMLData.details.functionType;
    var lines = [];
    lines.push(humanMessage("You are passing too many arguments to this function. The function has type:"));
    var reType = reasonifyType(Reasonify.niceifyType(type), Math.round(Reasonify.config.columns / 2));
    lines.push(codeMessage(error(reType)));
    var resolution = {
      intro: null,
      help: [
        {
          title: "Did you forget a semicolon or comma?",
          example:
            "If you forget to end a statement/let binding with a semicolon, it might look " +
            "like you are applying too many arguments to a function. The same thing can " +
            "happen if you forget a comma inside of a tuple or inside of a list/array."
        }
      ]
    };
    lines.push('');
    lines.push('');
    lines.push(renderHelpSection(resolution));
    lines.push('');
    return lines.join('\n');
  },
  'TypeErrors.RecordFieldNotInExpression': function(diagnostic) {
    var expressionType = diagnostic.commonMLData.details.expressionType;
    var fieldName = diagnostic.commonMLData.details.fieldName;
    var belongToType =diagnostic.commonMLData.details.belongToType;
    var hint = diagnostic.commonMLData.details.hint;

    var reExpressionType = reasonifyType(Reasonify.niceifyType(expressionType), Math.round(Reasonify.config.columns / 2));
    var reBelongToType = reasonifyType(Reasonify.niceifyType(belongToType), Math.round(Reasonify.config.columns / 2));

    var lines = [];
    lines.push(humanMessage("The expression to the left of ." + fieldName + " has type :"));
    lines.push(codeMessage(success(reExpressionType)));
    lines.push(humanMessage("But the field " + error(fieldName) + " does not belong to type:"));

    lines.push(codeMessage(success(reBelongToType)));
    lines.push('');
    lines.push('');
    var hint = hint && hint.trim();
    if (hint) {
      var resolution = {
        intro: "Hint:",
        help: [
          {
            title: hint.trim(),
            example: ""
          }
        ]
      };
      lines.push(renderHelpSection(resolution));
    }
    lines.push('');
    return lines.join('\n');
  },
  'TypeErrors.RecordFieldError': function(diagnostic) {
    var recordType = diagnostic.commonMLData.details.recordType;
    var fieldName = diagnostic.commonMLData.details.fieldName;
    var belongToType = diagnostic.commonMLData.details.belongToType;
    var thingThatIsMismatched = diagnostic.commonMLData.details.fieldOrConstructor;
    var hint = diagnostic.commonMLData.details.hint;

    var reRecordType = reasonifyType(Reasonify.niceifyType(recordType), Math.round(Reasonify.config.columns / 2));
    var reBelongToType = reasonifyType(Reasonify.niceifyType(belongToType), Math.round(Reasonify.config.columns / 2));

    var lines = [];
    lines.push(humanMessage("This record expression is expected to have type:"));
    lines.push(codeMessage(success(reRecordType)));
    lines.push('');
    lines.push(humanMessage("But the " + thingThatIsMismatched + " " + error(fieldName) + " does not belong to type:"));
    lines.push(codeMessage(success(reBelongToType)));
    if (diagnostic.commonMLData.details.hint) {
      lines.push(humanMessage(success(diagnostic.commonMLData.details.hint)));
    }
    var resolution = {
      intro: "Here are some things you can try:",
      help: []
    };
    if (thingThatIsMismatched === 'field') {
      resolution.help.push({
        title: "Add field " + fieldName + " to " + reBelongToType,
        example: ""
      });
    }
    lines.push('');
    lines.push('');
    lines.push(renderHelpSection(resolution));
    lines.push('');
    return lines.join('\n');
  },
  'TypeErrors.UnboundModule': function(diagnostic) {
    var lines = [];
    var moduleName = diagnostic.commonMLData.details.moduleName;
    var modulePath = moduleName.split('.');
    var mainMessage =
      "Unable to find the module named " + errorUnderline(moduleName) + ".";
    lines.push(humanMessage(mainMessage));
    var resolution = {
      intro: "Possible Explanations:",
      help: []
    };
    if (modulePath.length === 1) {
      resolution.help.push({
        title: "The file " + lowerBase(moduleName) + ".re might be missing from your package:",
        example:
          "If you define a file called " + lowerBase(moduleName) + ".re then other files " +
          "within your package can reference it as " + moduleName + ". Did you spell the module " +
          "name correctly? Is the file in your project's source directory (src)?"
      });
      resolution.help.push({
        title: moduleName + " might be missing a dependency:",
        example:
          "If " + moduleName + " is the package namespace of an intended dependency " +
          "it might be that you haven't installed it correctly."
      });
    } else {
      var remainingModulePath = modulePath.slice(1).join('\.');
      resolution.help.push({
        title: "You might have forgotten to define " +  moduleName + " inside of " + modulePath[0],
        example: ""
      });
      resolution.help.push({
        title: "You might have forgotten to publicly export " +  remainingModulePath + " from dependency " + (modulePath[0]) + ":",
        example:
         "If " + modulePath[0] + " is one of your package dependencies then " + modulePath[0] + " might " +
         "have forgotten to define a file named " + lowerBase(modulePath[1]) + ".re or " +
         "it forgot to include " + modulePath[1] + " in its package.json 'CommonML.exports' field. (The 'exports' " +
         "field lists the set of modules that can be accessed by dependencies)."
      });
    }
    lines.push('');
    lines.push('');
    lines.push(renderHelpSection(resolution));
    lines.push('');
    return lines.join('\n');
  },
  'TypeErrors.UnboundValue': function(diagnostic) {
    var lines = [];
    var expression = diagnostic.commonMLData.details.expression;
    var splitNamespacedExpression = expression.split('\.');
    var finalName = splitNamespacedExpression[splitNamespacedExpression.length - 1];
    var valuePath = splitNamespacedExpression.slice(0, splitNamespacedExpression.length - 1);
    var mainMessage =
      splitNamespacedExpression.length === 1 ?
      "I cannot find the value " + errorUnderline(expression) + " in scope." :
      "I cannot find the value " + errorUnderline(finalName);
    lines.push(humanMessage(mainMessage));
    if (diagnostic.commonMLData.details.hint) {
      lines.push(humanMessage(success(diagnostic.commonMLData.details.hint)));
    }
    var resolution = {
      intro: "In order to refer to a value by name, it must either be in \"scope\" or you must say where it can be found. Here are some things to try:",
      help: []
    };
    var properPrefix =
      splitNamespacedExpression.length === 1 ?
      {
        title: "Make sure " + finalName + " is defined by a let binding in scope:",
        example: ""
      } :
      {
        title: "Check that " + finalName + " is actually defined in " + valuePath.join('\.') + ":",
        example:
          "Perhaps " + finalName + " is not actually defined in " + valuePath.join('\.') + "."
      };
    resolution.help.push(properPrefix);

    if (splitNamespacedExpression.length === 1) {
      resolution.help.push({
        title: "Try \"Opening\" the correct module inside of " + path.basename(diagnostic.filePath) + ":",
        example:
          "If the let binding for '" + finalName +
          "' is in module Your.Module, then you can write \"open Your.Module;\" at the top of " +
          "the file. " +
          "You can also open modules locally inside of a function. " +
          "Understand that this will bring more than just this one let binding into scope."
      });
    }
    resolution.help.push({
      title: "Make sure " + finalName + " isn't made hidden by an interface file :",
      example:
        "If the let binding for " + finalName + " is located in another file, make sure that any interface files " +
       "also include the let binding so that it will be made public."
    });
    lines.push('');
    lines.push('');
    lines.push(renderHelpSection(resolution));
    return lines.join('\n');
  },
  'TypeErrors.UnboundConstructor': function(diagnostic) {
    var lines = [];
    var namespacedConstructor = Reasonify.niceifyType(diagnostic.commonMLData.details.namespacedConstructor);
    var splitNamespacedConstructor = namespacedConstructor.split('\.');
    var constructor = splitNamespacedConstructor[splitNamespacedConstructor.length - 1];
    var constructorPath = splitNamespacedConstructor.slice(0, splitNamespacedConstructor.length - 1);
    var mainMessage =
      "I cannot find the definition of the constructor " + errorUnderline(namespacedConstructor) + ".";
    lines.push(humanMessage(mainMessage));
    if (diagnostic.commonMLData.details.hint) {
      lines.push(humanMessage(success(diagnostic.commonMLData.details.hint)));
    }
    var properPrefix =
      splitNamespacedConstructor.length === 1 ?
      {
        title: "Prefix the constructor with correct module:",
        example:
          "You can specify where to find " + constructor + " by prefixing " +
          "the constructor with the module that it was defined in. " +
          "If the type containing '" + constructor + "' was defined in Your.Module, you can " +
          "write:  Your.Module." + constructor
      } :
      {
        title: "Check that " + constructor + " is actually defined in " + constructorPath.join('\.') + ":",
        example:
          "Perhaps " + constructor + " is not actually defined in " + constructorPath.join('\.') + "."
      };
    var resolution = {
      intro: "In order to refer to a Constructor, it must either be in \"scope\" or you must say where it can be found. Here are some things to try:",
      help: [properPrefix]
    };

    if (splitNamespacedConstructor.length === 1) {
      resolution.help.push({
        title: "Try \"Opening\" the correct module inside of " + path.basename(diagnostic.filePath) + ":",
        example:
          "If the type definition containing the constructor '" + constructor +
          "' is in module Your.Module, then you can write \"open Your.Module;\" at the top of " +
          "the file to make the constructor '" + constructor + "' visible. " +
          "You can also open modules locally inside of a function. " +
          "Understand that you're bringing more than just the constructor names into scope - you're bringing " +
          "everything in Your.Module into scope."
      });
    }
    resolution.help.push({
      title: "Make sure there isn't some interface file hiding the type definition that contains " + constructor + ":",
      example:
        "Wherever " + constructor + " is defined, make sure that any interface files " +
        "*also* redefine that same type with the same set of constructors. If there are no " +
        "interface files, then that's probably not the issue."
    });
    lines.push('');
    lines.push('');
    lines.push(renderHelpSection(resolution));
    lines.push('');
    return lines.join('\n');
  },
  'TypeErrors.UnboundTypeConstructor': function(diagnostic) {
    var lines = [];
    var namespacedConstructor = Reasonify.niceifyType(diagnostic.commonMLData.details.namespacedConstructor);
    var splitNamespacedConstructor = namespacedConstructor.split('\.');
    var type = splitNamespacedConstructor[splitNamespacedConstructor.length - 1];
    var constructorPath = splitNamespacedConstructor.slice(0, splitNamespacedConstructor.length - 1);
    var mainMessage =
      "I cannot find the definition of the type " + errorUnderline(namespacedConstructor) + ".";
    lines.push(humanMessage(mainMessage));
    if (diagnostic.commonMLData.details.hint) {
      lines.push(humanMessage(success(diagnostic.commonMLData.details.hint)));
    }
    var properPrefix =
      splitNamespacedConstructor.length === 1 ?
      {
        title: "Prefix the type with correct module:",
        example:
          "You can specify where to find " + type + " by prefixing " +
          "the type with the module that it was defined in. " +
          "If the type '" + type + "' was defined in Your.Module, you can " +
          "write:  Your.Module." + type
      } :
      {
        title: "Check that " + type + " is actually defined in " + constructorPath.join('\.') + ":",
        example:
          "Perhaps " + type + " is not actually defined in " + constructorPath.join('\.') + "."
      };
    var resolution = {
      intro: "In order to refer to a type, it must either be in \"scope\" or you must say where it can be found. Here are some things to try:",
      help: [properPrefix]
    };

    if (splitNamespacedConstructor.length === 1) {
      resolution.help.push({
        title: "Try \"Opening\" the correct module inside of " + path.basename(diagnostic.filePath) + ":",
        example:
          "If the type '" + type +
          "' is in module Your.Module, then you can write \"open Your.Module;\" at the top of " +
          "the file to make type '" + type + "' visible. " +
          "You can also open modules locally inside of a function. " +
          "Understand that you're bringing more than just the type into scope - you're bringing " +
          "everything in Your.Module into scope."
      });
    }
    resolution.help.push({
      title: "Make sure there isn't some interface file hiding the type definition that contains " + type + ":",
      example:
        "Wherever " + type + " is defined, make sure that any interface files " +
        "*also* redefine that same type. The interface files don't have to specify what the type is equal to - " +
        "they can simply include `type " + type + ";`. If there are no " +
        "interface files, then that's probably not the issue."
    });
    lines.push('');
    lines.push('');
    lines.push(renderHelpSection(resolution));
    lines.push('');
    return lines.join('\n');
  },
  'TypeErrors.UnboundRecordField': function(diagnostic) {
    var lines = [];
    var fieldName = Reasonify.niceifyType(diagnostic.commonMLData.details.fieldName);
    var mainMessage =
      "I don't know what type this field " + errorUnderline(fieldName) + " belongs to.";
    lines.push(humanMessage(mainMessage));
    if (diagnostic.commonMLData.details.hint) {
      lines.push(humanMessage(success(diagnostic.commonMLData.details.hint)));
    }
    var resolution = {
      intro: "Here are some things you can try:",
      help: [
        {
          title: "Prefix the field with correct module",
          example:
            "You can specify where to find the type that the field belongs to by prefixing " +
            "the field itself with the correct module path. " +
            "If the record containing '" + fieldName + "' was defined in Your.Module, you can " +
            "write:  {Your.Module." + fieldName + ": someValue}"
        },
        {
          title: "Try \"Opening\" the correct module inside of " + path.basename(diagnostic.filePath),
          example:
            "If the definition for the field '" + fieldName +
            "' is in module Your.Module, then you can write \"open Your.Module;\" at the top of " +
            "the file to make the field '" + fieldName + "' visible. " +
            "You can also open modules locally inside of a function. " +
            "Understand that you're bringing more than just the record field names into scope - you're bringing " +
            "everything in Your.Module into scope."
        },
        {
          title: "Double Check Your Code",
          example: "Is the field name '" + fieldName + "' actually correct? Did you mispell it?"
        }
      ]
    };
    lines.push('');
    lines.push('');
    lines.push(renderHelpSection(resolution));
    lines.push('');
    return lines.join('\n');
  }
};


var formatter = function(errors) {
  try {
    var lines = [];
    for (var i = 0; i < errors.length; i++) {
      var diagnostic = errors[i];
      if (diagnostic.scope === 'file') {
        if (Reasonify.config.showFileHeaders) {
          lines.push(renderFileHeader(diagnostic));
        }
        if (diagnostic.commonMLData) {
          var previewExpand = diagnostic.type === 'ERROR' ? Reasonify.config.errorPreviewExpand : Reasonify.config.warningPreviewExpand;
          var sourcePreview =
            diagnostic.commonMLData.fileText ?
            renderSourcePreview(diagnostic.type, previewExpand, diagnostic.commonMLData.fileText, diagnostic.range) :
            'No File Text To Preview';

          if (Reasonify.config.showSourcePreviews) {
            lines.push(sourcePreview);
          }
          if (KindPrinters[diagnostic.commonMLData.kind]) {
            lines.push(KindPrinters[diagnostic.commonMLData.kind](diagnostic));
          } else {
            lines.push('');
            lines.push('Something went wrong while pretty printing/extracting errors. We will show the raw error message. But you should make sure everything is updated (build system error printer/project):');
            lines.push(error(diagnostic.text));
          }
        } else {
          lines.push('Something went wrong while pretty printing/extracting errors. We will show the raw error message. But you should make sure everything is updated (build system error printer/project):');
          lines.push(error(diagnostic.text));
        }
      } else {
        if (Reasonify.config.showFileHeaders) {
          lines.push(renderFileHeader(diagnostic));
        }
        lines.push(error('Project Error'));
        lines.push(error(diagnostic.text));
      }
    }
    return lines.join('\n');
  } catch (e) {
    logError("!!!!!!!!!!");
    logError("Pretty printing the stderr threw an exception. Please copy/paste the verbose stderr log output into a Github issue so that we can repro the failure.");
    logError("!!!!!!!!!!");
    throw e;
  }
};

module.exports = formatter;
