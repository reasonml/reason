const fs = require('fs');
const cp = require('child_process');
const path = require('path');

if (process.cwd() !== path.resolve(__dirname, '..')) {
  console.log("ERROR: Must run `make esy-prepublish` from project root.");
  process.exit(1);
}

let projectRoot = process.cwd();

let relativeSubpackages = [];
for (var i = 2; i < process.argv.length; i++) {
  let relativeSubpackage = process.argv[i];
  relativeSubpackages.push(relativeSubpackage);
}

if (relativeSubpackages.length === 0) {
  relativeSubpackages = ['.'];
}

for (var i = 0; i < relativeSubpackages.length; i++) {
  let relativeSubpackage = relativeSubpackages[i];
  let subpackageRoot = path.resolve(projectRoot, relativeSubpackage);
  if (!relativeSubpackage || !fs.existsSync(subpackageRoot)) {
    console.log(
      'You specified an invalid release package root (' +
      subpackageRoot +
      '). Specify location of packages to release relative to repo root directory.'
    );
    process.exit(1);
  }
  if (!fs.existsSync(path.resolve(subpackageRoot, 'esy.json'))) {
    console.log(
      'You specified a package release root (' +
      subpackageRoot +
      ') that does not contain a package.json file'
    );
    process.exit(1);
  }
}

let releaseRoot = path.resolve(projectRoot, '_release');
if (fs.existsSync(releaseRoot)) {
  console.log('YOU NEED TO REMOVE THE ' + releaseRoot + ' DIR FIRST!');
  process.exit(1);
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

// Files to copy from subpackages to root, and therefore delete if present
let copyOver = {
  '.npmignore': '.npmignore',
  'package.json': 'package.json',
  'esy.json': 'package.json'
};

process.chdir(projectRoot);
let tarResult = cp.spawnSync('tar', ['--exclude', 'node_modules', '--exclude', '_build', '--exclude', '.git', '-cf', 'template.tar', '.']);
let tarErr = tarResult.stderr.toString();
// if (tarErr !== '') {
  // console.log('ERROR: Could not create template npm pack for prepublish');
  // throw new Error('Error:' + tarErr);
// }

try {
  let _releaseDir = path.resolve(projectRoot, '_release');
  cp.spawnSync('mkdir', ['-p', _releaseDir]);

  // For each subpackage, we release the entire source code for all packages, but
  // with the root package.json swapped out with the esy.json file in the
  // subpackage.
  for (var i = 0; i < relativeSubpackages.length; i++) {
    process.chdir(projectRoot);
    let relativeSubpackage = relativeSubpackages[i];
    let subpackageRoot = path.resolve(projectRoot, relativeSubpackage);
    let subpackageReleaseDir = path.resolve(_releaseDir, relativeSubpackage);
    let subpackageReleasePrepDir = path.resolve(_releaseDir, path.join(relativeSubpackage), '_prep');
    cp.spawnSync('mkdir', ['-p', subpackageReleaseDir]);
    cp.spawnSync('mkdir', ['-p', subpackageReleasePrepDir]);
    cp.spawnSync(
      'cp',
      [
        path.join(projectRoot, 'template.tar'),
        path.join(subpackageReleasePrepDir, 'template.tar')
      ]
    );
    process.chdir(subpackageReleasePrepDir);
    cp.spawnSync('tar', ['-xvf', 'template.tar']);
    cp.spawnSync('rm', [path.join(subpackageReleasePrepDir, 'template.tar')]);
    const packageJsonPath = path.resolve(subpackageRoot, 'esy.json');
    const packageJson = require(packageJsonPath);
    const packageName = packageJson.name;
    const packageVersion = packageJson.version;

    // In the process we want to remove any .npmignore/package/esy.json files
    // that were at the root, before we even copy subpackage files to the root.
    for (var filename in copyOver) {
      let destFile = path.resolve(subpackageReleasePrepDir, filename);
      if (fs.existsSync(destFile)) {
        let rmResult = cp.spawnSync('rm', [destFile]);
        let mvErr = rmResult.stderr.toString();
        if (mvErr !== '') {
          console.log('ERROR: Could not rm ' + filename + ' - ' + mvErr);
          process.exit(1);
        }
      }
    }

    for (var filename in copyOver) {
      let originPath = path.resolve(subpackageRoot, filename);
      let destPath = path.resolve(subpackageReleasePrepDir, copyOver[filename]);
      if (fs.existsSync(originPath)) {
        let cpResult = cp.spawnSync('cp', [originPath, destPath]);
        let mvErr = cpResult.stderr.toString();
        if (mvErr !== '') {
          console.log('ERROR: Could not move ' + filename + ' - ' + mvErr);
          process.exit(1);
        }
      }
    }
    
    // Create a npm pack to remove all the stuff in .npmignore.  This would
    // happen when you publish too, but we'll create a directory ./package that
    // has all of it removed so you can also easily test linking against it
    // from other projects.
    process.chdir(subpackageReleasePrepDir);
    // Npm pack is just a convenient way to strip out any unnecessary files.
    let packResult = cp.spawnSync('npm', ['pack']);
    let packStatus = packResult.status;
    let packErr = packResult.stderr.toString();
    if (packStatus !== 0) {
      console.log('ERROR: Could not create npm pack for ' + subpackageReleasePrepDir);
      throw new Error('Error:' + packErr);
    } else {
      if (packErr !== "") {
        console.warn('INFO: stderr output while running npm pack for ' + subpackageReleasePrepDir);
        console.log(packErr);
      }
    }
    
    let mvFrom = '*.tgz';
    let mvTo = subpackageReleaseDir;
    let mvResult = cp.spawnSync('mv', [mvFrom, mvTo], {shell: true});
    var mvErr = mvResult.stderr.toString();
    if (mvErr !== '') {
      console.log('ERROR: Could not move from ' + mvFrom + ' to ' + mvTo);
      throw new Error('Error:' + mvErr);
    }
    process.chdir(mvTo);
    let tarResult = cp.spawnSync('tar', ['-xvf', '*.tgz'], {shell: true});
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
  cp.spawnSync('rm', [ path.join(projectRoot, 'template.tar')]);
}
