'use babel';
/* @flow */

/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the license found in the LICENSE file in
 * the root directory of this source tree.
 */

export const RE_GRAMMARS = Object.freeze(['source.reason']);

const identifierOrNumber = '[a-zA-Z0-9_$]+';

function makeStrRegex(delimiter: string): string {
  const d = delimiter;
  // Each run of four backslashes ends up as just one backslash. We need to escape once for the
  // string literal here, and once for the RegExp compilation.
  return `${d}(\\\\.|[^${d}\\\\])*${d}`;
}

const strRegexes = ['`', "'", '"'].map(makeStrRegex);

const regexStrings = [].concat(strRegexes, [identifierOrNumber]).map(s => `(${s})`);

export const RE_WORD_REGEX = new RegExp(regexStrings.join('|'), 'g');
