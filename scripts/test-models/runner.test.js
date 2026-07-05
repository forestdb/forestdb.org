'use strict';

const assert = require('assert');
const test = require('node:test');
const { isBrowserOnlyBox } = require('./runner');

test('allows only documented browser-dependent boxes', () => {
  assert.strictEqual(isBrowserOnlyBox('elephants.md', 8), true);
  assert.strictEqual(isBrowserOnlyBox('elephants.md', 11), true);
  assert.strictEqual(isBrowserOnlyBox('elephants_continuized.md', 7), true);
  assert.strictEqual(isBrowserOnlyBox('liquid_physics.md', 1), true);
});

test('does not mask failures in other boxes or files', () => {
  assert.strictEqual(isBrowserOnlyBox('elephants.md', 7), false);
  assert.strictEqual(isBrowserOnlyBox('elephants_continuized.md', 6), false);
  assert.strictEqual(isBrowserOnlyBox('unrelated.md', 1), false);
});
