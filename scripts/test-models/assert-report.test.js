'use strict';

const assert = require('assert');
const test = require('node:test');
const { reportProblems, unexpectedFailures } = require('./assert-report');

const passing = { filename: 'passing.md', failures: [] };
const knownTimeout = {
  filename: 'adj-order-appendix.md',
  failures: [{ box: 8, timedOut: true, error: 'timeout after 120s' }],
};
const compileFailure = {
  filename: 'broken.md',
  failures: [{ box: 1, timedOut: false, error: 'ReferenceError: x is not defined' }],
};

test('accepts passing models', () => {
  assert.deepStrictEqual(unexpectedFailures([passing]), []);
});

test('accepts the documented heavy-box timeout only', () => {
  assert.deepStrictEqual(unexpectedFailures([knownTimeout]), []);
});

test('rejects compile failures', () => {
  assert.deepStrictEqual(unexpectedFailures([compileFailure]), [compileFailure]);
});

test('rejects a different failure in the baseline-exception page', () => {
  const failure = {
    filename: 'adj-order-appendix.md',
    failures: [{ box: 7, timedOut: false, error: 'SyntaxError' }],
  };
  assert.deepStrictEqual(unexpectedFailures([failure]), [failure]);
});

test('rejects additional failures alongside the known timeout', () => {
  const failure = {
    filename: 'adj-order-appendix.md',
    failures: [
      knownTimeout.failures[0],
      { box: 2, timedOut: false, error: 'ReferenceError' },
    ],
  };
  assert.deepStrictEqual(unexpectedFailures([failure]), [failure]);
});

test('rejects empty and incomplete reports', () => {
  assert.deepStrictEqual(reportProblems([], ['a.md']), ['report is empty', 'missing result for a.md']);
  assert.deepStrictEqual(reportProblems([passing], ['passing.md', 'missing.md']), ['missing result for missing.md']);
});

test('accepts only the two documented no-code skips', () => {
  const allowed = {
    filename: 'plural-predication.md',
    skipped: 'no runnable code boxes',
  };
  assert.deepStrictEqual(reportProblems([allowed], ['plural-predication.md']), []);
  const unexpected = { filename: 'passing.md', skipped: 'no webppl binary for v9' };
  assert.deepStrictEqual(
    reportProblems([unexpected], ['passing.md']),
    ['passing.md was skipped: no webppl binary for v9'],
  );
});

test('rejects duplicate and unexpected report entries', () => {
  assert.deepStrictEqual(
    reportProblems([passing, passing], ['passing.md']),
    ['duplicate result for passing.md'],
  );
  assert.deepStrictEqual(
    reportProblems([passing], []),
    ['unexpected result for passing.md'],
  );
});
