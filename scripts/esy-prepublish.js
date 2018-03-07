const fs = require('fs');
const cp = require('child_process');
const path = require('path');

if (process.cwd() !== path.resolve(__dirname, '..')) {
  console.log("ERROR: Must run `make esy-prepublish` from project root.");
  process.exit(1);
}

let projectRoot = process.cwd();

if (fs.existsSync(path.resolve(projectRoot, '_release'))) {
  console.log('YOU NEED TO REMOVE THE _release DIR FIRST!');
  process.exit(1);
}

const head =
  cp.spawnSync('git', ['rev-parse', '--verify', 'HEAD']).stdout.toString();
const master =
  cp.spawnSync('git', ['rev-parse', '--verify', 'master']).stdout.toString();

if (master !== head) {
  console.log('');
  console.log('ERROR: You are not on your local master branch. You should generally keep a local');
  console.log('master branch in sync with origin/master, and then publish from your local master');
  console.log('branch without any local changes. This makes sure you publish what has been committed');
  console.log('');
  console.log('Make sure origin/master is at the commit you want to publish, then run this command');
  console.log('to make your local master branch to point to origin/master:');
  console.log('');
  console.log('    git branch -f master origin/master');
  console.log('');
  process.exit(1);
}

let uncommitted =
  cp.spawnSync('git', ['diff-index', 'HEAD', '--']).stdout.toString();

if (uncommitted !== "") {
  console.log('ERROR: You have uncommitted changes. Please try on a clean master branch');
  process.exit(1);
}

let backupFiles = {
  '.npmignore': '.backup.npmignore.backup',
  'package.json': '.backup.package.json.backup',
  'esy.json': '.backup.esy.json.backup'
};

for (var fileName in backupFiles) {
  var backupName = backupFiles[fileName];
  let mvResult = cp.spawnSync(
    'mv',
    [path.resolve(projectRoot, fileName), backupName]
  );

  let mvErr = mvResult.stderr.toString();
  if (mvErr !== '') {
    console.log('ERROR: Could not back up ' + fileName + ' - ' + mvErr);
    process.exit(1);
  }
}

try {
  let packages = ['reason', 'rtop', 'rebuild'];
  for (var i = 0; i < packages.length; i++) {
    process.chdir(projectRoot);
    const packageJson =  require('./esy/esy.' + packages[i] + '.json');
    const packageStr =  JSON.stringify(packageJson, null, 2);
    let copyFrom = path.resolve(projectRoot, 'scripts', 'esy', 'esy.' + packages[i] + '.json');
    let copyTo = path.resolve(projectRoot, 'package.json');
    let cpResult = cp.spawnSync('cp', [copyFrom, copyTo]);
    let cpErr = cpResult.stderr.toString();
    if (cpErr !== '') {
      console.log('ERROR: Could not copy from ' + copyFrom + ' to ' + copyTo);
      throw new Error('Error:' + cpErr);
    }
    let packResult = cp.spawnSync('npm', ['pack']);
    let packErr = cpResult.stderr.toString();
    if (packErr !== '') {
      console.log('ERROR: Could not create npm pack for ' + packages[i]);
      throw new Error('Error:' + packErr);
    }
    let _releaseDir = path.resolve(projectRoot, '_release');
    cp.spawnSync('mkdir', ['-p', _releaseDir]);
    let packageReleaseDir = path.resolve(_releaseDir, packages[i]);
    cp.spawnSync('mkdir', ['-p', packageReleaseDir]);
    let mvFrom = 'esy-ocaml-' + packages[i] + '*';
    let mvTo = packageReleaseDir;
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

  }
  console.log('SUCCESS!');
  console.log('--------');
  console.log('Now you can cd package && npm publish. That will actually publish to npm so be careful.');
  console.log('Unfortunately, in the process of preparing for publishing');
  console.log('we\'ve changed some files locally in the project. Sorry! reset them in git.');
  for (var i = 0; i < packages.length; i++) {
    var package = packages[i];
    console.log('    cd ' + path.join(projectRoot, '_release', package, 'package'));
    console.log('    npm publish --access=public');
  }
} finally {
  for (var fileName in backupFiles) {
    var backupName = backupFiles[fileName];
    let mvResult = cp.spawnSync(
      'mv',
      [path.resolve(projectRoot, backupName), path.resolve(projectRoot, fileName)]
    );

    let mvErr = mvResult.stderr.toString();
    if (mvErr !== '') {
      console.log('ERROR: Could not restore backup for ' + fileName + ' - ' + mvErr);
    }
  }
}
