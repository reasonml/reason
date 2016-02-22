'use babel';
/* @flow */

/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the license found in the LICENSE file in
 * the root directory of this source tree.
 */

import {JAVASCRIPT_WORD_REGEX} from '../lib/constants.js';

describe('JAVASCRIPT_WORD_REGEX', () => {
  function getAllMatches(r: RegExp, s: string): Array<string> {
    // Reset the state of the RegExp.
    r.lastIndex = 0;
    const results = [];
    let currentResult;
    while ((currentResult = r.exec(s)) != null) {
      results.push(currentResult[0]);
    }
    return results;
  }

  // For brevity in specs.
  function matches(s) {
    return getAllMatches(JAVASCRIPT_WORD_REGEX, s);
  }

  it('should match numbers', () => {
    expect(matches('454     1231')).toEqual(['454', '1231']);
  });

  it('should match identifiers', () => {
    expect(matches('hello these are $_words___A (mostly)')).toEqual([
      'hello',
      'these',
      'are',
      '$_words___A',
      'mostly',
    ]);
  });

  ['`', "'", '"'].forEach(delimiter => {
    describe(`matching strings delimited by ${delimiter}.`, () => {
      // For brevity.
      const d = delimiter;
      it('should match a simple string', () => {
        expect(matches(`${d}asdf asdf${d} identifier ${d}another string${d}`)).toEqual([
          `${d}asdf asdf${d}`,
          'identifier',
          `${d}another string${d}`,
        ]);
      });

      it('should handle escaped delimiters', () => {
        expect(matches(`id ${d}foo \\${d} bar${d} another id`)).toEqual([
          'id',
          `${d}foo \\${d} bar${d}`,
          'another',
          'id',
        ]);
        expect(matches(`${d}\\${d}${d}`)).toEqual([`${d}\\${d}${d}`]);
      });

      it('should handle backslashes in front of other characters', () => {
        expect(matches(`${d}\\4asdf foo${d}`)).toEqual([`${d}\\4asdf foo${d}`]);
        expect(matches(`${d}\\\\${d}`)).toEqual([`${d}\\\\${d}`]);
      });
    });
  });
});
