'use strict';

const assert = require('assert');
const fs = require('fs');
const os = require('os');
const path = require('path');
const test = require('node:test');
const { indexProblems, internalLinkProblems } = require('./check-built-site');

const models = [
  { title: 'Visible', url: 'https://forestdb.org/models/visible.html', status: 'code' },
  { title: 'Hidden', url: 'https://forestdb.org/models/hidden.html', status: 'hidden' },
];

test('accepts one index link per visible model and none for hidden models', () => {
  const html = '<a href="/models/visible.html">Visible</a>';
  assert.deepStrictEqual(indexProblems(models, html), []);
});

test('rejects missing visible models', () => {
  assert.deepStrictEqual(indexProblems(models, ''), ['Visible appears 0 times on the index; expected 1']);
});

test('rejects duplicate visible models', () => {
  const link = '<a href="/models/visible.html">Visible</a>';
  assert.deepStrictEqual(indexProblems(models, link + link), ['Visible appears 2 times on the index; expected 1']);
});

test('rejects hidden models rendered on the index', () => {
  const html = '<a href="/models/visible.html">Visible</a><a href="/models/hidden.html">Hidden</a>';
  assert.deepStrictEqual(indexProblems(models, html), ['Hidden appears 1 time on the index; expected 0']);
});

test('accepts root-relative, relative, directory, and fragment links that exist', () => {
  const site = fs.mkdtempSync(path.join(os.tmpdir(), 'forest-site-'));
  fs.mkdirSync(path.join(site, 'models'));
  fs.mkdirSync(path.join(site, 'assets'));
  fs.writeFileSync(path.join(site, 'index.html'), '<a href="/models/page.html">model</a>');
  fs.writeFileSync(path.join(site, 'assets', 'site.css'), '');
  fs.writeFileSync(
    path.join(site, 'models', 'page.html'),
    '<a href="../">home</a><link href="/assets/site.css"><a href="#section">section</a>',
  );
  assert.deepStrictEqual(internalLinkProblems(site), []);
});

test('reports missing internal links with the source page', () => {
  const site = fs.mkdtempSync(path.join(os.tmpdir(), 'forest-site-'));
  fs.writeFileSync(path.join(site, 'index.html'), '<a href="/missing.html">missing</a>');
  assert.deepStrictEqual(internalLinkProblems(site), ['index.html links to missing target /missing.html']);
});

test('ignores external, mail, data, and javascript URLs', () => {
  const site = fs.mkdtempSync(path.join(os.tmpdir(), 'forest-site-'));
  fs.writeFileSync(
    path.join(site, 'index.html'),
    '<a href="https://example.com">web</a><a href="mailto:x@example.com">mail</a>' +
      '<img src="data:image/png;base64,x"><a href="javascript:void(0)">js</a>',
  );
  assert.deepStrictEqual(internalLinkProblems(site), []);
});
