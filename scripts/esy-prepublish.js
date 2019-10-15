/**
 * Copyright 2004-present Facebook. All Rights Reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//this file was copied from https://github.com/facebook/reason/blob/master/scripts/esy-prepublish.js
//
//  Usage: Run from the repo root:
//
//    node scripts/esy-prepublish.js relative/path/to/some-package-name.json
//  The script will copy relative/path/to/some-package-name.json into
//  ./package.json and delete any remaining esy.json at the root. It will also
//  search for relative/path/to/some-package-name.README.md (or if that is not
//  found, then relative/path/to/README.md) and copy it to ./README.md at the
//  repo root so that the published package has its appropriate README on the
//  npm page.
const fs = require('fs');
const cp = require('child_process');
const path = require('path');

const quote = s => '"' + s + '"';
const opamifyName = name => {
  if(name.indexOf("@opam/") === 0) {
    return name.substr(6);
  } else if(name === '@esy-ocaml/reason') {
    return "reason";
  } else {
    if(name.indexOf('@') === 0) {
      var scopeAndPackage = name.substr(1).split('/');
      // return 'npm--' + scopeAndPackage[0] + '--' + scopeAndPackage[1];
      // Assumes the packages have name.opam files, without the scope.
      return scopeAndPackage[1];
    } else {
      return name;
    }
  }
};
const opamifyVersion = v => {
  var v = v.trim();
  if(v.charAt(0) === '^') {
    var postCaret = v.substr(1);
    var nextDotIndex = postCaret.indexOf('.');
    if(nextDotIndex !== -1) {
      var major = postCaret.substr(0, nextDotIndex);
      var rest = postCaret.substr(nextDotIndex + 1);
      return '>= "' + postCaret + '" & < "' + (parseInt(major) + 1) + '.0.0"';
    } else {
      var major = postCaret.substr(0, nextDotIndex);
      return '>= "' + postCaret + '" & < "' + (parseInt(postCaret) + 1) + '"';
    }
  } else {
    return v.replace(/\s+<\s+/g, s => '" & < "')
        .replace(/\s+<=\s*/g, s => '" & <= "')
        .replace(/^<\s+/g, s => '< "')
        .replace(/^<=\s*/g, s => '<= "')

        .replace(/\s+>\s+/g, s => '" & > "')
        .replace(/\s+>=/g, s => '" & >= "')
        .replace(/^>\s+/g, s => '> "')
        .replace(/^>=\s*/g, s => '>= "')
        + '"';
  }
};
const depMap = (o) => {
  return Object.entries(o).map(([name, vers]) =>
    opamifyName(name) +
      (vers === '*' ? '' : '   {' + opamifyVersion(vers) + '}')
  )
};
const createOpamText = package => {
  const opamTemplate = [
    'opam-version: ' + quote(package.version),
    'maintainer: ' + quote(package.author),
    'authors: [' + quote(package.author) + ']',
    'license: ' + quote(package.license),
    'homepage: ' + quote(package.homepage),
    'doc: ' + quote(package.homepage),
    package.repository && package.repository.url ?
      ('bug-reports: ' + quote(package.repository.url)) :
      '',
    package.repository && package.repository.url ?
      ('dev-repo: ' + quote(package.repository.url)) :
      '',
    'tags: [' + (package.keywords ? package.keywords.map(quote).join(' ') : '') + ']',
    'build: [ [' + package.esy.build.split(' ').map(quote).join(' ') + ' ] ]',
    'depends: [',
  ].concat(depMap(package.dependencies).map(s=>'  ' + s)).concat([
    ']',
    'synopsis: ' + quote(package.description),
    'description: ' + quote(package.description)
  ]);
  return opamTemplate.join('\n') + '\n';
};


if (process.cwd() !== path.resolve(__dirname, '..')) {
  console.log("ERROR: Must run `make esy-prepublish` from project root.");
  process.exit(1);
}

let projectRoot = process.cwd();

let relativeJsonPaths = [];
for (var i = 2; i < process.argv.length; i++) {
  let jsonRelativePath = process.argv[i];
  relativeJsonPaths.push(jsonRelativePath);
}

if (relativeJsonPaths.length === 0) {
  relativeJsonPaths = ['esy.json'];
}

for (var i = 0; i < relativeJsonPaths.length; i++) {
  let jsonRelativePath = relativeJsonPaths[i];
  let subpackageJson = path.resolve(projectRoot, jsonRelativePath);
  if (path.extname(jsonRelativePath) !== '.json') {
    console.log(
      'You specified an relative path to something that isn\'t a json file (' +
      subpackageJson +
      '). Specify location of json files relative to repo root.'
    );
    process.exit(1);
  }
  if (!jsonRelativePath || !fs.existsSync(subpackageJson)) {
    console.log(
      'You specified an invalid release package root (' +
      subpackageJson +
      '). Specify location of packages to release relative to repo root directory.'
    );
    process.exit(1);
  }
}

const head =
  cp.spawnSync('git', ['rev-parse', '--verify', 'HEAD']).stdout.toString();
const master =
  cp.spawnSync('git', ['rev-parse', '--verify', 'master']).stdout.toString();

// Since we generate opam files, don't check for uncommitted.
// let uncommitted =
//   cp.spawnSync('git', ['diff-index', 'HEAD', '--']).stdout.toString();

// if (uncommitted !== "") {
//   console.log('ERROR: You have uncommitted changes. Please try on a clean master branch');
//   process.exit(1);
// }

process.chdir(projectRoot);
let tarResult = cp.spawnSync(
  'tar',
  [
    '--exclude',
    '_esy',
    '--exclude',
    'node_modules',
    '--exclude',
    '_build',
    '--exclude',
    '.git',
    '-cf',
    'template.tar',
    '.'
  ]
);
let tarErr = tarResult.stderr.toString();
// if (tarErr !== '') {
// console.log('ERROR: Could not create template npm pack for prepublish');
// throw new Error('Error:' + tarErr);
// }

try {
  let _releaseDir = path.resolve(projectRoot, '_release');

  // For each subpackage, we release the entire source code for all packages, but
  // with the root package.json swapped out with the esy.json file in the
  // subpackage.
  for (var i = 0; i < relativeJsonPaths.length; i++) {
    process.chdir(projectRoot);
    let jsonRelativePath = relativeJsonPaths[i];
    let jsonResolvedPath = path.resolve(projectRoot, jsonRelativePath);
    const packageJson = require(jsonResolvedPath);
    const packageName = packageJson.name;
    const packageVersion = packageJson.version;
    console.log('');
    console.log('Preparing: ' + jsonRelativePath + ' ' + packageName + '@' + packageVersion);
    console.log('-----------------------------------------------------------------------------');

    let subpackageReleaseDir = path.resolve(_releaseDir, jsonRelativePath);
    if (fs.existsSync(subpackageReleaseDir)) {
      console.log('YOU NEED TO REMOVE THE ' + subpackageReleaseDir + ' DIR FIRST!');
      process.exit(1);
    }
    if (!fs.existsSync(_releaseDir)) {
      fs.mkdirSync(_releaseDir);
    }
    fs.mkdirSync(subpackageReleaseDir);
    let subpackageReleasePrepDir = path.resolve(_releaseDir, path.join(jsonRelativePath), '_prep');
    fs.mkdirSync(subpackageReleasePrepDir);
    fs.copyFileSync(
      path.join(projectRoot, 'template.tar'),
      path.join(subpackageReleasePrepDir, 'template.tar')
    );
    process.chdir(subpackageReleasePrepDir);
    cp.spawnSync('tar', ['-xvf', 'template.tar']);
    fs.unlinkSync(path.join(subpackageReleasePrepDir, 'template.tar'));

    let readmePath = path.resolve(subpackageReleasePrepDir, 'README.md');
    let readmePkgPath =
      path.resolve(
        subpackageReleasePrepDir,
        path.join('src', path.basename(jsonRelativePath, '.json'), 'README.md')
      );
    let readmeResolvedPath =
      fs.existsSync(readmePkgPath) ? readmePkgPath :
        fs.existsSync(readmePath) ? readmePath :
          null;

    let toCopy = [
      {
        originPath: path.resolve(subpackageReleasePrepDir, jsonRelativePath),
        destPath: path.resolve(subpackageReleasePrepDir, 'package.json')
      },
      {
        originPath: readmeResolvedPath,
        destPath: path.resolve(subpackageReleasePrepDir, 'README.md')
      }
    ];
    for (var j = 0; j < toCopy.length; j++) {
      let originPath = toCopy[j].originPath;
      let destPath = toCopy[j].destPath;
      if (originPath !== null && fs.existsSync(originPath) && destPath !== originPath) {
        fs.renameSync(originPath, destPath);
      }
    }

    // If an esy.json file remains, we need to remove it so that it isn't
    // picked up as the default by esy (it gives priority to esy.json over
    // package.json). But this has to be done _after_ the `mv` above, in case
    // the json file that someone published _was_ the esy.json file.
    let esyFile = path.resolve(subpackageReleasePrepDir, 'esy.json');
    if (fs.existsSync(esyFile)) {
      fs.unlinkSync(esyFile);
    }

    // Create a npm pack to remove all the stuff in .npmignore.  This would
    // happen when you publish too, but we'll create a directory ./package that
    // has all of it removed so you can also easily test linking against it
    // from other projects.
    process.chdir(subpackageReleasePrepDir);
    // Npm pack is just a convenient way to strip out any unnecessary files.
    const packResult = cp.spawnSync(process.platform === 'win32' ? 'npm.cmd' : 'npm', ['pack']);
    if (packResult.status !== 0) {
      console.log('ERROR: Could not create npm pack for ' + subpackageReleasePrepDir);
      throw new Error('Error:' + packResult.stderr.toString());
    }
    const mvTo = subpackageReleaseDir;
    fs.readdirSync(subpackageReleasePrepDir).filter(fn => fn.endsWith('.tgz')).forEach(fn => {
      fs.renameSync(fn, path.join(mvTo, fn));
    });
    process.chdir(mvTo);
    const tarResult = cp.spawnSync('tar', ['-xvf', '*.tgz'], { shell: true });
    if (tarResult.error) {
      console.log('ERROR: Could not untar in ' + mvTo);
      throw new Error('Error:' + tarResult.stderr.toString());
    }
    console.log('Prepared for publishing at: ');
    console.log('  ' + subpackageReleaseDir);
    if(packageJson['esy-prepublish-generate-opam']) {
      try {
        const opamText = createOpamText(packageJson);
        const opamFileName = path.basename(jsonRelativePath, '.json') + '.opam';
        let opamResolvedPath = path.resolve(projectRoot, opamFileName);
        fs.writeFileSync(opamResolvedPath, opamText);
        console.log("Opam file generated. Commit it. Or don't:");
        console.log('  ' + opamResolvedPath);
      } catch(e) {
        console.log("Could not generate opam file. See error below.");
        console.log(
          "To disable opam file generation, remove `\"esy-prepublish-generate-opam\": true` from " +
          jsonRelativePath
        );
        console.log('  ' + e.toString());
      }
    } else {
      console.log("To generate opam file, add `\"esy-prepublish-generate-opam\": true` to " + jsonRelativePath);
    }
    console.log('To publish the package to npm do:');
    console.log('  cd ' + path.resolve(subpackageReleaseDir, 'package'));
    console.log('  npm publish --access=public');
    console.log('');
  }
} finally {
  fs.unlinkSync(path.join(projectRoot, 'template.tar'));
}
