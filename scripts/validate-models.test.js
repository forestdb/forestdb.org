'use strict';

const assert = require('assert');
const fs = require('fs');
const os = require('os');
const path = require('path');
const test = require('node:test');
const {
  VALID_CATEGORIES,
  parseFrontmatter,
  normalizeTitle,
  computeBodyHash,
  checkOpeningParagraph,
  findGenericProse,
  loadPolicy,
  validateFile,
  validateCorpus,
} = require('./validate-models');

const FIXTURES_DIR = path.join(__dirname, 'validate-fixtures', 'models');
const FIXTURES_POLICY_PATH = path.join(__dirname, 'validate-fixtures', 'editorial-policy.json');

function resultFor(results, file) {
  const result = results.find((r) => r.file === file);
  assert.ok(result, `expected a result for ${file}`);
  return result;
}

test('parseFrontmatter splits frontmatter from body and strips quotes', () => {
  const raw = '---\ntitle: "Hello World"\nmodel-status: code\n---\nbody text\n';
  const parsed = parseFrontmatter(raw);
  assert.strictEqual(parsed.frontmatter.title, 'Hello World');
  assert.strictEqual(parsed.frontmatter['model-status'], 'code');
  assert.strictEqual(parsed.body, 'body text\n');
});

test('parseFrontmatter returns null for a missing frontmatter block', () => {
  assert.strictEqual(parseFrontmatter('no frontmatter here'), null);
});

test('normalizeTitle collapses case, punctuation, and whitespace', () => {
  assert.strictEqual(normalizeTitle('Shared Fixture Title'), 'shared fixture title');
  assert.strictEqual(normalizeTitle('shared   fixture title!!'), 'shared fixture title');
});

test('computeBodyHash is deterministic and content-sensitive', () => {
  const a = computeBodyHash('same body\n');
  const b = computeBodyHash('same body\n');
  const c = computeBodyHash('different body\n');
  assert.strictEqual(a, b);
  assert.notStrictEqual(a, c);
  assert.strictEqual(a.length, 64);
});

test('checkOpeningParagraph accepts a real intro paragraph', () => {
  const body = '\nThis page infers a latent rate from a sufficiently long sequence of coin flips.\n\n~~~~\nflip(0.5)\n~~~~\n';
  const shape = checkOpeningParagraph(body);
  assert.strictEqual(shape.ok, true);
});

test('checkOpeningParagraph rejects code starting immediately', () => {
  const body = '\n~~~~\nflip(0.5)\n~~~~\n';
  const shape = checkOpeningParagraph(body);
  assert.strictEqual(shape.ok, false);
  assert.match(shape.reason, /missing opening paragraph/);
});

test('checkOpeningParagraph rejects a too-short paragraph', () => {
  const body = '\nA coin model.\n\n~~~~\nflip(0.5)\n~~~~\n';
  const shape = checkOpeningParagraph(body);
  assert.strictEqual(shape.ok, false);
  assert.match(shape.reason, /too short/);
});

test('checkOpeningParagraph rejects a heading-only opener', () => {
  const body = '\n# Title\n\n~~~~\nflip(0.5)\n~~~~\n';
  const shape = checkOpeningParagraph(body);
  assert.strictEqual(shape.ok, false);
});

test('checkOpeningParagraph skips attribution lines before looking for prose', () => {
  const body = '\n*By Fixture Author*\n\nThis page infers a latent rate from a sufficiently long sequence of coin flips.\n\n~~~~\nflip(0.5)\n~~~~\n';
  const shape = checkOpeningParagraph(body);
  assert.strictEqual(shape.ok, true);
});

test('checkOpeningParagraph rejects indented Church code', () => {
  const body = '\n    (define coin (flip 0.5))\n    (display coin)\n';
  const shape = checkOpeningParagraph(body);
  assert.strictEqual(shape.ok, false);
  assert.match(shape.reason, /code/);
});

test('checkOpeningParagraph rejects a list before any prose', () => {
  const body = '\n- first item in a long list\n- second item in the same list\n';
  const shape = checkOpeningParagraph(body);
  assert.strictEqual(shape.ok, false);
  assert.match(shape.reason, /list/);
});

test('checkOpeningParagraph rejects a link-only opening', () => {
  const body = '\n[External implementation](https://example.com/some-model)\n';
  const shape = checkOpeningParagraph(body);
  assert.strictEqual(shape.ok, false);
  assert.match(shape.reason, /link-only/);
});

test('checkOpeningParagraph rejects more than three sentences', () => {
  const body = '\nOne sentence introduces the model. A second explains its data. A third states the inference. A fourth is too much.\n';
  const shape = checkOpeningParagraph(body);
  assert.strictEqual(shape.ok, false);
  assert.match(shape.reason, /too many sentences/);
});

test('checkOpeningParagraph rejects an overly long introduction', () => {
  const words = Array.from({ length: 91 }, (_, i) => `word${i}`).join(' ');
  const shape = checkOpeningParagraph(`${words}.\n`);
  assert.strictEqual(shape.ok, false);
  assert.match(shape.reason, /too long/);
});

test('findGenericProse flags known boilerplate phrases and nothing else', () => {
  assert.deepStrictEqual(
    findGenericProse('This model demonstrates a generative process.'),
    ['this model demonstrates'],
  );
  assert.deepStrictEqual(
    findGenericProse('This model infers a latent rate from observed flips.'),
    [],
  );
});

test('loadPolicy reads the fixture policy and returns its protected pages', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  assert.strictEqual(policy.protectedPages.length, 3);
});

test('loadPolicy rejects a missing policy file', () => {
  assert.throws(
    () => loadPolicy(path.join(__dirname, 'does-not-exist.json')),
    /does not exist/,
  );
});

test('loadPolicy rejects unsupported policy metadata', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'forest-policy-'));
  const policyPath = path.join(dir, 'policy.json');
  fs.writeFileSync(policyPath, JSON.stringify({ version: 2, hashAlgorithm: 'md5', protectedPages: [] }));
  assert.throws(() => loadPolicy(policyPath), /version 1/);
});

test('validateFile passes a well-formed editable page with no failures or warnings', () => {
  const raw = fs.readFileSync(path.join(FIXTURES_DIR, 'valid-editable.md'), 'utf8');
  const result = validateFile('valid-editable.md', raw, undefined, {});
  assert.deepStrictEqual(result.failures, []);
  assert.deepStrictEqual(result.warnings, []);
});

test('validateFile rejects unknown model languages and WebPPL versions', () => {
  const body = 'This paragraph contains enough words to serve as a concise and useful model introduction.\n';
  const unknownLanguage = `---\nlayout: model\ntitle: Bad Language\nmodel-status: code\nmodel-language: typo\nmodel-category: Regression and Statistical Learning\n---\n${body}`;
  const badVersion = `---\nlayout: model\ntitle: Bad Version\nmodel-status: code\nmodel-language: webppl\nmodel-language-version: v99\nmodel-category: Regression and Statistical Learning\n---\n${body}`;
  assert.ok(validateFile('bad-language.md', unknownLanguage, undefined, {}).failures.some((f) => /model-language/.test(f)));
  assert.ok(validateFile('bad-version.md', badVersion, undefined, {}).failures.some((f) => /model-language-version/.test(f)));
});

test('validateFile permits Terra only for static listings', () => {
  const body = 'This paragraph contains enough words to serve as a concise and useful model introduction.\n';
  const raw = `---\nlayout: model\ntitle: Terra Code\nmodel-status: code\nmodel-language: terra\nmodel-category: Scientific and Physical Models\n---\n${body}`;
  assert.ok(validateFile('terra-code.md', raw, undefined, {}).failures.some((f) => /Terra/.test(f)));
});

test('validateFile requires introductions on maintained link and hidden pages', () => {
  const link = '---\nlayout: model\ntitle: Link\nmodel-status: link\nmodel-category: Scientific and Physical Models\n---\n[Code](https://example.com)\n';
  const hidden = '---\nlayout: model\ntitle: Hidden\nmodel-status: hidden\nmodel-language: webppl\nmodel-category: Probability and Bayesian Data Analysis\n---\n~~~~\nflip(0.5)\n~~~~\n';
  assert.ok(validateFile('link.md', link, undefined, {}).failures.some((f) => /opening paragraph/.test(f)));
  assert.ok(validateFile('hidden.md', hidden, undefined, {}).failures.some((f) => /opening paragraph/.test(f)));
});

test('every entry in VALID_CATEGORIES is a plain non-empty string', () => {
  assert.ok(VALID_CATEGORIES.length === 9);
  for (const category of VALID_CATEGORIES) {
    assert.strictEqual(typeof category, 'string');
    assert.ok(category.length > 0);
  }
});

test('validateCorpus: valid editable fixture has no failures', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  assert.deepStrictEqual(resultFor(results, 'valid-editable.md').failures, []);
});

test('validateCorpus: protected page with a matching hash passes even with no intro', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'valid-protected.md');
  assert.deepStrictEqual(result.failures, []);
  assert.strictEqual(result.isProtected, true);
});

test('validateCorpus: protected page with a mismatched hash fails', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'protected-tampered.md');
  assert.ok(result.failures.some((f) => /body hash mismatch/.test(f)));
});

test('validateCorpus: policy entry missing from disk is reported as a failure', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'missing-from-disk.md');
  assert.ok(result.failures.some((f) => /missing from models directory/.test(f)));
});

test('validateCorpus: invalid category fails', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'invalid-category.md');
  assert.ok(result.failures.some((f) => /invalid model-category/.test(f)));
});

test('validateCorpus: missing category fails', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'missing-category.md');
  assert.ok(result.failures.some((f) => /missing model-category/.test(f)));
});

test('validateCorpus: missing status fails', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'missing-status.md');
  assert.ok(result.failures.some((f) => /missing required frontmatter field: model-status/.test(f)));
});

test('validateCorpus: invalid status fails', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'invalid-status.md');
  assert.ok(result.failures.some((f) => /invalid model-status/.test(f)));
});

test('validateCorpus: missing language fails for a runnable status', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'bad-language.md');
  assert.ok(result.failures.some((f) => /missing model-language/.test(f)));
});

test('validateCorpus: missing language is fine for status link', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'ok-link-no-language.md');
  assert.deepStrictEqual(result.failures, []);
});

test('validateCorpus: tags are never required', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'no-tags-fixture.md');
  assert.deepStrictEqual(result.failures, []);
});

test('validateCorpus: normalized duplicate titles fail on both files', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const a = resultFor(results, 'duplicate-title-a.md');
  const b = resultFor(results, 'duplicate-title-b.md');
  assert.ok(a.failures.some((f) => /duplicate normalized title/.test(f)));
  assert.ok(b.failures.some((f) => /duplicate normalized title/.test(f)));
});

test('validateCorpus: missing intro fails by default (strict)', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'missing-intro.md');
  assert.ok(result.failures.some((f) => /invalid opening paragraph/.test(f)));
  assert.deepStrictEqual(result.warnings, []);
});

test('validateCorpus: --allow-intro-failures downgrades missing intro to a warning', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, { allowIntroFailures: true });
  const result = resultFor(results, 'missing-intro.md');
  assert.deepStrictEqual(result.failures, []);
  assert.ok(result.warnings.some((w) => /invalid opening paragraph/.test(w)));
});

test('validateCorpus: too-short intro fails by default', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'short-intro.md');
  assert.ok(result.failures.some((f) => /too short/.test(f)));
});

test('validateCorpus: heading-only intro fails by default', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'heading-only-intro.md');
  assert.ok(result.failures.some((f) => /invalid opening paragraph/.test(f)));
});

test('validateCorpus: generic prose warns but does not fail', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'generic-prose.md');
  assert.deepStrictEqual(result.failures, []);
  assert.ok(result.warnings.some((w) => /generic prose pattern/.test(w)));
});

test('validateCorpus: hidden status with language present passes', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'hidden-with-language.md');
  assert.deepStrictEqual(result.failures, []);
});

test('validateCorpus: malformed frontmatter fails', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results } = validateCorpus(FIXTURES_DIR, policy, {});
  const result = resultFor(results, 'malformed-frontmatter.md');
  assert.ok(result.failures.some((f) => /malformed frontmatter/.test(f)));
});

test('validateCorpus: summary counts add up across the fixture corpus', () => {
  const policy = loadPolicy(FIXTURES_POLICY_PATH);
  const { results, summary } = validateCorpus(FIXTURES_DIR, policy, {});
  assert.strictEqual(summary.filesChecked, results.filter((r) => r.file !== 'missing-from-disk.md').length);
  assert.strictEqual(
    summary.totalFailures,
    results.reduce((sum, r) => sum + r.failures.length, 0),
  );
  assert.ok(summary.totalFailures > 0, 'fixture corpus is expected to contain intentional failures');
});
