// TEST
var child_process = require('child_process');

var defaultConfig = {
  // Amount to expand source preview in each direction. If -1, shows no preview
  pathToReasonfmt: 'reasonfmt',
  showSourcePreviews: true,
  // Whether or not to show the file path/line in the error formatting
  showFileHeaders: true,
  errorPreviewExpand: 5,
  warningPreviewExpand: 2,
  indentCode: 4,
  indentHuman: 2,
  niceifyModuleAliases: true,
  renderHtml: false,
  columns: process.stdout.columns
};
exports.config = defaultConfig;
exports.setConfig = function(c) {
  exports.config = c;
};
exports.removeBlankLines = function(lines) {
  var ret = [];
  for (var i = 0; i < lines.length; i++) {
    if (lines[i].trim() !== '') {
      ret.push(lines[i]);
    };
  }
  return ret;
};

// Removes left padding and trims right
exports.removeLeftPadding = function(lines) {
  var smallestLeftPadding = 1000;
  for (var i = 0; i < lines.length; i++) {
    var lineToShow = lines[i];
    if (lineToShow !== '') {
      var leftPadding = lineToShow.length - lineToShow.trimLeft().length;
      smallestLeftPadding = Math.min(smallestLeftPadding, leftPadding);
    }
  }
  var ret = [];
  for (var i = 0; i < lines.length; i++) {
    ret.push(lines[i].slice(smallestLeftPadding).trimRight());
  }
  return {removedLeftPadding:smallestLeftPadding, lines:ret};
};

exports.reasonifyManySignatureItems = function(items, width) {
  // Careful to add a couple of extra spaces. The ML -> Reason printer isn't as
  // accurate as the Reason -> Reason printer.
  var mlText = items.join(' (*SPLIT*) ');
  var resplit = child_process.execSync(
    exports.config.pathToReasonfmt + ' -is-interface-pp true -use-stdin true -parse ml -print re -print-width ' + (width - 1),
    {input: mlText}
  ).toString().split('/*SPLIT*/');
  return resplit;
};


exports.reasonifyTypeLines = function(mlType, width) {
  var mlType = "type dummy = " + mlType;
  return exports.removeBlankLines(
    exports.removeLeftPadding(
      exports.replaceDummyType(
        child_process.execSync(exports.config.pathToReasonfmt + ' -use-stdin true -parse ml -print re -print-width ' + (width - 1),
          {input: mlType}
        ).toString()
      ).split('\n')
    ).lines
  );
};

exports.niceifyType = function(txt) {
  var fixedModuleAliases = !exports.config.niceifyModuleAliases ?
    txt :
    txt.replace(/M_(\w*)__(\w*)/g, function(match, packageName, moduleName) {
      return packageName + '.' + moduleName;
    });
  var niceified =
    fixedModuleAliases.replace(/(\w*)#(\d)/g, function(match, letter, number) {
      return "'exists_" + letter + number;
    });
  return niceified;
};
exports.niceifyTypes = function(types) {
  return types.map(function(type) {return exports.niceifyType(type);});
};
exports.replaceDummyType = function(txt) {
  var withoutDummy = txt.replace(/(type dummy =)/g, function(match) {
    return'';
  }).trimRight();
  if (withoutDummy[withoutDummy.length -1] === ';') {
    return withoutDummy.slice(0, withoutDummy.length - 1);
  } else {
    return withoutDummy;
  }
};

exports.reasonifyEquivalentTypeLines = function(mlTypes, width) {
  return mlTypes.map(function(typ){return exports.reasonifyTypeLines(typ, width); });
};

exports.reasonifyType = function(mlType, width) {
  return exports.reasonifyTypeLines(mlType, width).join('\n').trimRight();
};

