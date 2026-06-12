#!/usr/bin/env node
// Headless smoke tests for the webppl models on forestdb.org.
//
// For every model with `model-language: webppl`, extracts each runnable code
// box and executes it with the webppl version the model declares
// (`model-language-version`, default v0.9.15). Browser-only globals (`viz`,
// `editor`) are stubbed out. Produces a markdown report on stdout and a JSON
// report at report.json.
//
// Usage: node runner.js [--timeout <seconds>] [--concurrency <n>] [--only <substring>]
//
// Exit code is always 0; this is a report, not a gate.

'use strict';

const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFile } = require('child_process');

const ROOT = path.resolve(__dirname, '..', '..');
const MODELS_DIR = path.join(ROOT, 'models');
const STUBS = ['viz', 'editor', 'print', 'vizPrint'].map((s) => path.join(__dirname, 'stubs', s));

const VERSION_PACKAGES = {
  'pre-v0.7': 'webppl-0-6-1',
  'v0.9.6': 'webppl-0-9-6',
  'v0.9.7': 'webppl-0-9-7',
  'v0.9.9': 'webppl-0-9-9',
  'v0.9.13': 'webppl-0-9-13',
  'v0.9.15': 'webppl-0-9-15',
};
const DEFAULT_VERSION = 'v0.9.15';

const args = process.argv.slice(2);
function argValue(name, fallback) {
  const i = args.indexOf(name);
  return i >= 0 ? args[i + 1] : fallback;
}
const TIMEOUT_MS = parseInt(argValue('--timeout', '60'), 10) * 1000;
const CONCURRENCY = parseInt(argValue('--concurrency', String(Math.max(2, os.cpus().length - 2))), 10);
const ONLY = argValue('--only', null);

function parseFrontmatter(text) {
  const m = text.match(/^---\n([\s\S]*?)\n---\n/);
  if (!m) return [{}, text];
  const fm = {};
  for (const line of m[1].split('\n')) {
    const kv = line.match(/^([A-Za-z-]+):\s*(.*)$/);
    if (kv) fm[kv[1]] = kv[2].trim();
  }
  return [fm, text.slice(m[0].length)];
}

// Extract fenced code blocks (~~~ or ```), skipping ones marked norun.
function extractCodeBoxes(body) {
  const boxes = [];
  const re = /^(~{3,}|`{3,})([^\n]*)\n([\s\S]*?)^\1\s*$/gm;
  let m;
  while ((m = re.exec(body)) !== null) {
    const info = m[2].trim();
    if (/norun/.test(info)) continue;
    boxes.push(m[3]);
  }
  return boxes;
}

function webpplBinary(version) {
  const pkg = VERSION_PACKAGES[version];
  if (!pkg) return null;
  const bin = path.join(__dirname, 'node_modules', pkg, 'webppl');
  return fs.existsSync(bin) ? bin : null;
}

function runBox(bin, code, tmpDir, label) {
  return new Promise((resolve) => {
    const file = path.join(tmpDir, label.replace(/[^\w.-]/g, '_') + '.wppl');
    fs.writeFileSync(file, code);
    const boxArgs = [bin, file];
    for (const stub of STUBS) boxArgs.push('--require', stub);
    execFile(process.execPath, boxArgs, { timeout: TIMEOUT_MS, maxBuffer: 16 * 1024 * 1024 },
      (error, stdout, stderr) => {
        if (!error) return resolve({ ok: true });
        const timedOut = error.killed || error.signal === 'SIGTERM';
        const clean = (stderr || error.message || '').replace(/\x1b\[[0-9;]*m/g, '');
        const lines = clean.split('\n');
        const firstError = lines.find((l) => /^\s*[A-Z][a-zA-Z]*Error\b|^\s*Error\b/.test(l)) ||
          lines.find((l) => /error/i.test(l) && !/throw error/.test(l)) || lines[0] || '';
        resolve({ ok: false, timedOut, error: timedOut ? `timeout after ${TIMEOUT_MS / 1000}s` : firstError.trim().slice(0, 300) });
      });
  });
}

async function testModel(filename, tmpDir) {
  const text = fs.readFileSync(path.join(MODELS_DIR, filename), 'utf8');
  const [fm, body] = parseFrontmatter(text);
  if (fm['model-language'] !== 'webppl') return null;
  const version = fm['model-language-version'] || DEFAULT_VERSION;
  const bin = webpplBinary(version);
  if (!bin) return { filename, version, skipped: `no webppl binary for ${version}` };
  const boxes = extractCodeBoxes(body);
  if (boxes.length === 0) return { filename, version, skipped: 'no runnable code boxes' };
  const results = [];
  for (let i = 0; i < boxes.length; i++) {
    results.push(await runBox(bin, boxes[i], tmpDir, `${filename}-${i}`));
  }
  return {
    filename,
    version,
    status: fm['model-status'] || null,
    boxes: results.length,
    failures: results
      .map((r, i) => ({ ...r, box: i + 1 }))
      .filter((r) => !r.ok)
      .map((r) => ({ box: r.box, timedOut: !!r.timedOut, error: r.error })),
  };
}

async function main() {
  let files = fs.readdirSync(MODELS_DIR).filter((f) => f.endsWith('.md')).sort();
  if (ONLY) files = files.filter((f) => f.includes(ONLY));
  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'forest-test-'));

  const results = [];
  let next = 0;
  async function worker() {
    while (next < files.length) {
      const file = files[next++];
      const r = await testModel(file, tmpDir);
      if (r) {
        results.push(r);
        const tag = r.skipped ? 'SKIP' : r.failures.length === 0 ? 'PASS' : 'FAIL';
        process.stderr.write(`${tag} ${file}\n`);
      }
    }
  }
  await Promise.all(Array.from({ length: CONCURRENCY }, worker));

  results.sort((a, b) => a.filename.localeCompare(b.filename));
  const tested = results.filter((r) => !r.skipped);
  const passed = tested.filter((r) => r.failures.length === 0);
  const failed = tested.filter((r) => r.failures.length > 0);
  const skipped = results.filter((r) => r.skipped);

  const lines = [];
  lines.push(`# Model smoke-test report`);
  lines.push('');
  lines.push(`Tested ${tested.length} webppl models (headless; \`viz\`/\`editor\` stubbed): ` +
    `**${passed.length} passed**, **${failed.length} failed**, ${skipped.length} skipped.`);
  lines.push('');
  lines.push(`A headless failure does not always mean the model is broken in the browser ` +
    `(timeouts and editor-specific features are common causes), but compile errors are real.`);
  lines.push('');
  if (failed.length > 0) {
    lines.push(`## Failures`);
    lines.push('');
    for (const r of failed) {
      lines.push(`- **${r.filename}** (${r.version}, status: ${r.status || 'none'}, ` +
        `${r.failures.length}/${r.boxes} boxes failed)`);
      for (const f of r.failures.slice(0, 3)) {
        lines.push(`  - box ${f.box}: ${f.error}`);
      }
    }
    lines.push('');
  }
  if (skipped.length > 0) {
    lines.push(`## Skipped`);
    lines.push('');
    for (const r of skipped) lines.push(`- ${r.filename}: ${r.skipped}`);
    lines.push('');
  }
  const report = lines.join('\n');
  console.log(report);
  fs.writeFileSync(path.join(__dirname, 'report.json'), JSON.stringify(results, null, 2));
  fs.writeFileSync(path.join(__dirname, 'report.md'), report);
}

main();
