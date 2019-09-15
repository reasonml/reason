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

let uncommitted =
  cp.spawnSync('git', ['diff-index', 'HEAD', '--']).stdout.toString();

if (uncommitted !== "") {
  console.log('ERROR: You have uncommitted changes. Please try on a clean master branch');
  process.exit(1);
}

process.chdir(projectRoot);
let tarResult = cp.spawnSync('tar', ['--exclude', 'node_modules', '--exclude', '_build', '--exclude', '.git', '-cf', 'template.tar', '.']);
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
    const packageJson = require(jsonResolvedPath);
    const packageName = packageJson.name;
    const packageVersion = packageJson.version;

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
    for (var i = 0; i < toCopy.length; i++) {
      let originPath = toCopy[i].originPath;
      let destPath = toCopy[i].destPath;
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
    console.log('');
    console.log(packageName + '@' + packageVersion + ' prepared for publishing at ' + subpackageReleaseDir);
    console.log('');
    console.log('To publish the package to npm do:');
    console.log('');
    console.log('    cd ' + path.resolve(subpackageReleaseDir, 'package'));
    console.log('    npm publish --access=public');
    console.log('');
  }
} finally {
  fs.unlinkSync(path.join(projectRoot, 'template.tar'));
}

