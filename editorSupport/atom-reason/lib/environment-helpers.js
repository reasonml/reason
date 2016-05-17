'use babel';

// Copyright (c) 2016 GitHub Inc.
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
import {spawnSync} from 'child_process';
import os from 'os';

// Gets a dump of the user's configured shell environment.
//
// Returns the output of the `env` command or `undefined` if there was an error.
function getRawShellEnv () {
  let shell = getUserShell();

  // The `-ilc` set of options was tested to work with the OS X v10.11
  // default-installed versions of bash, zsh, sh, and ksh. It *does not*
  // work with csh or tcsh.
  let results = spawnSync(shell, ['-ilc', 'env'], {encoding: 'utf8'});
  if (results.error || !results.stdout || results.stdout.length <= 0) {
    return;
  }

  return results.stdout;
}

function getUserShell () {
  if (process.env.SHELL) {
    return process.env.SHELL;
  }

  return '/bin/bash';
}

// Gets the user's configured shell environment.
//
// Returns a copy of the user's shell enviroment.
function getFromShell () {
  let shellEnvText = getRawShellEnv();
  if (!shellEnvText) {
    return;
  }

  let env = {};

  for (let line of shellEnvText.split(os.EOL)) {
    if (line.includes('=')) {
      let components = line.split('=');
      if (components.length === 2) {
        env[components[0]] = components[1];
      } else {
        let k = components.shift();
        let v = components.join('=');
        env[k] = v;
      }
    }
  }

  return env;
}

function needsPatching (options = { platform: process.platform, env: process.env }) {
  if (options.platform === 'darwin' && !options.env.PWD) {
    let shell = getUserShell();
    if (shell.endsWith('csh') || shell.endsWith('tcsh')) {
      return false;
    }
    return true;
  }

  return false;
}

function normalize (options = {}) {
  if (options && options.env) {
    process.env = options.env;
  }

  if (!options.env) {
    options.env = process.env;
  }

  if (!options.platform) {
    options.platform = process.platform;
  }

  if (needsPatching(options)) {
    // Patch the `process.env` on startup to fix the problem first documented
    // in #4126. Retain the original in case someone needs it.
    let shellEnv = getFromShell();
    if (shellEnv && shellEnv.PATH) {
      process._originalEnv = process.env;
      process.env = shellEnv;
    }
  }
}

export default { getFromShell, needsPatching, normalize };
