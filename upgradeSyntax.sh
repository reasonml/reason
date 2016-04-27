#!/bin/sh
# Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
# First arg is old refmt
# Second arg is new refmt
# Third arg is printwidth
die () {
  echo >&2 "$@"
  exit 1
}

[ "$#" -eq 3 ] || die "\nERROR:\n 3 arguments required [ oldPathToReason, newPathToReason, printWidth ], $# provided\n"



find . | grep "\.re$" | xargs -0 node -e "\
process.argv[1].split('\n').filter(function(n){return n;}).forEach(function(filePath) { \
  var binary = require('child_process').execSync('$1 ' + filePath + ' -print binary_reason');
  var reformatted = require('child_process').execSync(
     '$2 -use-stdin true -is-interface-pp false -parse binary_reason -print re -print-width $3 | sed -e \'s/ *\$//g\'',
     {input: binary}
  );
  require('fs').writeFileSync(filePath, reformatted);
}); \
"

find . | grep "\.rei$" | xargs -0 node -e "\
process.argv[1].split('\n').filter(function(n){return n;}).forEach(function(filePath) { \
  var binary = require('child_process').execSync('$1 ' + filePath + ' -print binary_reason');
  var reformatted = require('child_process').execSync(
    '$2 -use-stdin true -is-interface-pp true -parse binary_reason -print re -print-width $3 | sed -e \'s/ *\$//g\'',
    {input: binary}
  );
  require('fs').writeFileSync(filePath, reformatted);
}); \
console.log('echo \"Completed!\"'); \
"
