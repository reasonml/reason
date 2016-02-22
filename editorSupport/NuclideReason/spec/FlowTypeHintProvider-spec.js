'use babel';
/* @flow */

/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the license found in the LICENSE file in
 * the root directory of this source tree.
 */

const {uncachedRequire, spyOnGetterValue} = require('../../test-helpers');
const {Range} = require('atom');
const featureConfig = require('../../feature-config');

import {array} from '../../commons';

const TYPE_HINT_PROVIDER = '../lib/FlowTypeHintProvider';

describe('FlowTypeHintProvider', () => {
  const editor = {
    getPath() { return ''; },
    getText() { return ''; },
  };
  const position = [1, 1];
  const range = new Range([1, 2], [3, 4]);

  let typeHintProvider;

  afterEach(() => {
    // we assume here that runWith is called in every spec -- otherwise these
    // will not be spies
    jasmine.unspy(require('../../atom-helpers'), 'extractWordAtPosition');
    jasmine.unspy(featureConfig, 'get');
    jasmine.unspy(require('../../client'), 'getServiceByNuclideUri');
  });

  async function runWith(enabled, result, word) {
    spyOn(featureConfig, 'get').andCallFake(key => {
      if (key === 'nuclide-flow.enableTypeHints') {
        return enabled;
      } else {
        return false;
      }
    });
    spyOn(require('../../client'), 'getServiceByNuclideUri').andReturn({
      flowGetType() { return Promise.resolve(result); },
    });
    spyOnGetterValue(require('../../atom-helpers'), 'extractWordAtPosition')
      .andReturn(word);

    const {FlowTypeHintProvider} = (uncachedRequire(require, TYPE_HINT_PROVIDER): any);
    typeHintProvider = new FlowTypeHintProvider();
    return await typeHintProvider.typeHint(editor, position);
  }

  it('should return null when disabled', () => {
    waitsForPromise(async () => {
      expect(await runWith(false, {type: 'foo'}, {range})).toBe(null);
    });
  });

  it('should return the type', () => {
    waitsForPromise(async () => {
      expect((await runWith(true, {type: 'foo'}, {range})).hint).toBe('foo');
    });
  });

  it('should return the range', () => {
    waitsForPromise(async () => {
      expect((await runWith(true, {type: 'foo'}, {range})).range).toBe(range);
    });
  });

  it('should return null when the FlowService result is null', () => {
    waitsForPromise(async () => {
      expect(await runWith(true, null, {range})).toBe(null);
    });
  });

  it('should return a default range when the word is null', () => {
    waitsForPromise(async () => {
      expect((await runWith(true, {type: 'foo'}, null)).range)
        .toEqual(new Range(position, position));
    });
  });
});

describe('getTypeHintTree', () => {
  // $FlowIgnore
  const {getTypeHintTree} = require(TYPE_HINT_PROVIDER);

  function runWith(obj: Object): Object {
    return getTypeHintTree(JSON.stringify(obj));
  }

  function makeObjectType(props: Map<string, Object>): Object {
    const propTypes = [];
    for (const [name, prop] of props) {
      propTypes.push({
        name,
        type: prop,
      });
    }
    return {
      kind: 'ObjT',
      type: {
        propTypes,
      },
    };
  }

  function makeMaybeType(type: Object): Object {
    return {
      kind: 'MaybeT',
      type,
    };
  }

  function makeFunType(paramToType: Map<String, Object>, returnType: Object): Object {
    return {
      kind: 'FunT',
      funType: {
        paramNames: array.from(paramToType.keys()),
        paramTypes: array.from(paramToType.values()),
        returnType,
      },
    };
  }

  function makeArrayType(elemType: Object): Object {
    return {
      kind: 'ArrT',
      elemType,
    };
  }

  const num = { kind: 'NumT' };
  const str = { kind: 'StrT' };
  const bool = { kind: 'BoolT' };
  const anyObj = { kind: 'AnyObjT' };

  const emptyObject = makeObjectType(new Map());
  const simpleObject = makeObjectType(new Map([['numProp', num]]));
  const nestedObject = makeObjectType(new Map([['otherObj', simpleObject]]));

  const maybeString = makeMaybeType(str);
  const maybeObject = makeMaybeType(simpleObject);

  const simpleFunction = makeFunType(new Map(), num);
  const complexFunction = makeFunType(
    new Map([
      ['param1', simpleObject],
      ['param2', maybeString],
    ]),
    simpleObject,
  );

  const numArray = makeArrayType(num);
  const objArray = makeArrayType(simpleObject);

  it('should work for number', () => {
    expect(runWith(num)).toEqual({value: 'number'});
  });

  it('should work for string', () => {
    expect(runWith(str)).toEqual({value: 'string'});
  });

  it('should work for boolean', () => {
    expect(runWith(bool)).toEqual({value: 'boolean'});
  });

  it('should work for a raw Object', () => {
    expect(runWith(anyObj)).toEqual({value: 'Object'});
  });

  it('should work for an empty object', () => {
    expect(runWith(emptyObject)).toEqual({value: 'Object', children: []});
  });

  it('should work for a nonempty object', () => {
    expect(runWith(simpleObject)).toEqual({
      value: 'Object',
      children: [
        {
          value: 'numProp: number',
          children: undefined,
        },
      ],
    });
  });

  it('should work for a nested object', () => {
    expect(runWith(nestedObject)).toEqual({
      value: 'Object',
      children: [
        {
          value: 'otherObj: Object',
          children: [
            {
              value: 'numProp: number',
              children: undefined,
            },
          ],
        },
      ],
    });
  });

  it('should work with Arrays of primitives', () => {
    expect(runWith(numArray)).toEqual({
      value: 'Array<number>',
      children: undefined,
    });
  });

  it('should work with Arrays of Objects', () => {
    expect(runWith(objArray)).toEqual({
      value: 'Array<Object>',
      children: [
        {
          value: 'numProp: number',
          children: undefined,
        },
      ],
    });
  });

  it('should work with a simple maybe type', () => {
    expect(runWith(maybeString)).toEqual({
      value: '?string',
      children: undefined,
    });
  });

  it('should work with a a maybe object type', () => {
    expect(runWith(maybeObject)).toEqual({
      value: '?Object',
      children: [
        {
          value: 'numProp: number',
          children: undefined,
        },
      ],
    });
  });

  it('should work with a simple function', () => {
    expect(runWith(simpleFunction)).toEqual({
      value: 'Function',
      children: [
        {
          value: 'Parameters',
          children: [],
        },
        {
          value: 'Return Type: number',
          children: undefined,
        },
      ],
    });
  });

  it('should work with a more complicated function', () => {
    expect(runWith(complexFunction)).toEqual({
      value: 'Function',
      children: [
        {
          value: 'Parameters',
          children: [
            {
              value: 'param1: Object',
              children: [
                {
                  value: 'numProp: number',
                  children: undefined,
                },
              ],
            },
            {
              value: 'param2: ?string',
              children: undefined,
            },
          ],
        },
        {
          value: 'Return Type: Object',
          children: [
            {
              value: 'numProp: number',
              children: undefined,
            },
          ],
        },
      ],
    });
  });
});
