// TEST
var child_process = require('child_process');

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

exports.reasonifyManySignatureItems = function(items, width, config) {
  // Careful to add a couple of extra spaces. The ML -> Reason printer isn't as
  // accurate as the Reason -> Reason printer.
  var mlText = items.join(' (*SPLIT*) ');
  var resplit = child_process.execSync(
    config.pathToRefmt + ' -is-interface-pp true -use-stdin true -parse ml -print re -print-width ' + (width - 1),
    {input: mlText}
  ).toString().split('/*SPLIT*/');
  return resplit;
};


exports.reasonifyTypeLines = function(mlType, width, config) {
  var mlType = "type dummy = " + mlType;
  return exports.removeBlankLines(
    exports.removeLeftPadding(
      exports.replaceDummyType(
        child_process.execSync(config.pathToRefmt + ' -use-stdin true -parse ml -print re -print-width ' + (width - 1),
          {input: mlType}
        ).toString()
      ).split('\n')
    ).lines
  );
};

exports.niceifyType = function(txt, config) {
  var fixedModuleAliases = !config.niceifyModuleAliases ?
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
exports.niceifyTypes = function(types, config) {
  return types.map(function(type) {return exports.niceifyType(type, config);});
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

exports.reasonifyEquivalentTypeLines = function(mlTypes, width, config) {
  return mlTypes.map(function(typ){return exports.reasonifyTypeLines(typ, width, config); });
};

exports.reasonifyType = function(mlType, width, config) {
  return exports.reasonifyTypeLines(mlType, width, config).join('\n').trimRight();
};
