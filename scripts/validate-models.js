#!/usr/bin/env node
'use strict';

// Editorial and metadata validator for the models/ directory. No dependencies
// beyond Node builtins. See scripts/editorial-policy.json for the protected
// page registry and README.md for the frontmatter format this checks.

const fs = require('fs');
const path = require('path');
const crypto = require('crypto');

const VALID_STATUSES = ['code', 'static', 'link', 'hidden'];
const VALID_LANGUAGES = ['church', 'terra', 'webppl'];
const VALID_WEBPPL_VERSIONS = ['pre-v0.7', 'v0.9.6', 'v0.9.7', 'v0.9.9', 'v0.9.13', 'v0.9.15'];

// The nine-category taxonomy this validator enforces. Old categories such as
// "Miscellaneous" or "Uncategorized" are intentionally not in this list.
const VALID_CATEGORIES = [
  'Probability and Bayesian Data Analysis',
  'Graphical Models and Causality',
  'Regression and Statistical Learning',
  'Time Series and Stochastic Processes',
  'Bayesian Nonparametrics',
  'Program Induction and Concept Learning',
  'Language and Pragmatics',
  'Agents, Games, and Social Reasoning',
  'Scientific and Physical Models',
];

const MIN_INTRO_WORDS = 12;
const MAX_INTRO_WORDS = 90;
const MAX_INTRO_SENTENCES = 3;

// Non-exhaustive list of generic/boilerplate phrasing to warn about. This is
// a style nudge, not a quality judgment, so hits are always warnings and
// never failures.
const GENERIC_PROSE_PATTERNS = [
  'this model demonstrates',
  'this page demonstrates',
  'this model shows',
  'this model illustrates',
  'this is a simple example of',
  'in this model, we',
  'this generative model captures',
  'showcases',
];

const SKIP_LINE_PATTERNS = [
  /^\s*$/, // blank
  /^(-{3,}|_{3,})\s*$/, // horizontal rule / underline
  /^\*?\s*toc\s*$/i, // "* toc"
  /^\{:toc\}\s*$/i,
  /^#{0,6}\s*\*{0,2}(by|authors?|author)\b/i, // attribution lines
  /^<\/?script\b/i, // inline script tags (e.g. MathJax includes)
  /^#{1,6}\s+/, // markdown headings
];

const CODE_FENCE_RE = /^(~~~~|```)/;

function stripQuotes(value) {
  const trimmed = value.trim();
  if (trimmed.length >= 2) {
    const first = trimmed[0];
    const last = trimmed[trimmed.length - 1];
    if ((first === '"' && last === '"') || (first === "'" && last === "'")) {
      return trimmed.slice(1, -1);
    }
  }
  return trimmed;
}

function parseFrontmatter(raw) {
  const match = raw.match(/^---\r?\n([\s\S]*?)\r?\n---\r?\n?([\s\S]*)$/);
  if (!match) return null;
  const frontmatter = {};
  for (const line of match[1].split(/\r?\n/)) {
    const fieldMatch = line.match(/^([A-Za-z0-9_-]+):\s*(.*)$/);
    if (fieldMatch) {
      frontmatter[fieldMatch[1]] = stripQuotes(fieldMatch[2]);
    }
  }
  return { frontmatter, body: match[2] };
}

function normalizeTitle(title) {
  return (title || '')
    .toLowerCase()
    .normalize('NFKD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/[^a-z0-9]+/g, ' ')
    .trim()
    .replace(/\s+/g, ' ');
}

function computeBodyHash(body) {
  return crypto.createHash('sha256').update(body, 'utf8').digest('hex');
}

function checkOpeningParagraph(body) {
  const lines = body.split(/\r?\n/);
  let i = 0;
  while (i < lines.length && SKIP_LINE_PATTERNS.some((re) => re.test(lines[i]))) {
    i += 1;
  }
  if (i >= lines.length || CODE_FENCE_RE.test(lines[i])) {
    return { ok: false, reason: 'missing opening paragraph before the first code box' };
  }
  if (/^ {4}\S/.test(lines[i])) {
    return { ok: false, reason: 'indented code appears before an opening paragraph' };
  }
  if (/^\s*(?:[-+*]|\d+[.)])\s+/.test(lines[i])) {
    return { ok: false, reason: 'list appears before an opening paragraph' };
  }
  if (/^\s*\[[^\]]+\]\([^)]+\)\s*$/.test(lines[i])) {
    return { ok: false, reason: 'link-only block appears before an opening paragraph' };
  }
  const candidate = [];
  while (i < lines.length && lines[i].trim() !== '' && !CODE_FENCE_RE.test(lines[i])) {
    candidate.push(lines[i]);
    i += 1;
  }
  const paragraph = candidate.join(' ').trim();
  const wordCount = paragraph.split(/\s+/).filter(Boolean).length;
  if (wordCount < MIN_INTRO_WORDS) {
    return { ok: false, reason: `opening paragraph too short (${wordCount} words, need at least ${MIN_INTRO_WORDS})` };
  }
  if (wordCount > MAX_INTRO_WORDS) {
    return { ok: false, reason: `opening paragraph too long (${wordCount} words, maximum ${MAX_INTRO_WORDS})` };
  }
  const sentenceMatches = paragraph.match(/[.!?]["')\]]?(?=\s|$)/g);
  const sentenceCount = sentenceMatches ? sentenceMatches.length : 1;
  if (sentenceCount > MAX_INTRO_SENTENCES) {
    return { ok: false, reason: `opening paragraph has too many sentences (${sentenceCount}, maximum ${MAX_INTRO_SENTENCES})` };
  }
  return { ok: true, paragraph };
}

function findGenericProse(paragraph) {
  const lower = paragraph.toLowerCase();
  return GENERIC_PROSE_PATTERNS.filter((pattern) => lower.includes(pattern));
}

function loadPolicy(policyPath) {
  if (!fs.existsSync(policyPath)) {
    throw new Error(`editorial policy does not exist: ${policyPath}`);
  }
  const raw = fs.readFileSync(policyPath, 'utf8');
  const parsed = JSON.parse(raw);
  if (parsed.version !== 1) {
    throw new Error(`editorial policy must use version 1: ${policyPath}`);
  }
  if (parsed.hashAlgorithm !== 'sha256') {
    throw new Error(`editorial policy must use sha256 body hashes: ${policyPath}`);
  }
  if (!Array.isArray(parsed.protectedPages)) {
    throw new Error(`editorial policy at ${policyPath} has no protectedPages array`);
  }
  const seen = new Set();
  for (const entry of parsed.protectedPages) {
    if (!entry.file || seen.has(entry.file)) {
      throw new Error(`editorial policy has a missing or duplicate file entry: ${entry.file || '(missing)'}`);
    }
    seen.add(entry.file);
    if (entry.introExempt !== true || !entry.rationale) {
      throw new Error(`protected page ${entry.file} needs a rationale and introExempt=true`);
    }
    if (!/^[a-f0-9]{64}$/.test(entry.bodySha256 || '')) {
      throw new Error(`protected page ${entry.file} has an invalid sha256 body hash`);
    }
  }
  return parsed;
}

function policyByFile(policy) {
  const map = new Map();
  for (const entry of policy.protectedPages) {
    map.set(entry.file, entry);
  }
  return map;
}

function validateFile(file, raw, policyEntry, options) {
  const failures = [];
  const warnings = [];
  const parsed = parseFrontmatter(raw);

  if (!parsed) {
    failures.push('missing or malformed frontmatter block');
    return { file, failures, warnings, title: null, isProtected: Boolean(policyEntry) };
  }

  const { frontmatter, body } = parsed;

  if (!frontmatter.layout) failures.push('missing required frontmatter field: layout');
  if (!frontmatter.title) failures.push('missing required frontmatter field: title');

  const status = frontmatter['model-status'];
  if (!status) {
    failures.push('missing required frontmatter field: model-status');
  } else if (!VALID_STATUSES.includes(status)) {
    failures.push(`invalid model-status: "${status}" (expected one of ${VALID_STATUSES.join(', ')})`);
  }

  const language = frontmatter['model-language'];
  if (status !== 'link' && !language) {
    failures.push('missing model-language (required unless model-status is "link")');
  } else if (language && !VALID_LANGUAGES.includes(language)) {
    failures.push(`invalid model-language: "${language}"`);
  }
  if (language === 'terra' && status !== 'static') {
    failures.push('Terra pages must use model-status "static"');
  }
  const languageVersion = frontmatter['model-language-version'];
  if (languageVersion && language !== 'webppl') {
    failures.push('model-language-version is only valid for WebPPL pages');
  } else if (languageVersion && !VALID_WEBPPL_VERSIONS.includes(languageVersion)) {
    failures.push(`invalid model-language-version: "${languageVersion}"`);
  }

  const category = frontmatter['model-category'];
  if (!category) {
    failures.push('missing model-category');
  } else if (!VALID_CATEGORIES.includes(category)) {
    failures.push(`invalid model-category: "${category}" (expected one of the nine taxonomy categories)`);
  }

  const isProtected = Boolean(policyEntry);
  if (isProtected) {
    const bodyHash = computeBodyHash(body);
    if (policyEntry.bodySha256 !== bodyHash) {
      failures.push('protected page body hash mismatch (body edited without editorial review)');
    }
  } else {
    const shape = checkOpeningParagraph(body);
    if (!shape.ok) {
      const message = `invalid opening paragraph: ${shape.reason}`;
      if (options.allowIntroFailures) {
        warnings.push(message);
      } else {
        failures.push(message);
      }
    } else {
      for (const hit of findGenericProse(shape.paragraph)) {
        warnings.push(`generic prose pattern: "${hit}"`);
      }
    }
  }

  return { file, failures, warnings, title: frontmatter.title, isProtected };
}

function validateCorpus(modelsDir, policy, options) {
  options = options || {};
  const files = fs
    .readdirSync(modelsDir)
    .filter((f) => f.endsWith('.md'))
    .sort();
  const fileSet = new Set(files);
  const byFile = policyByFile(policy);

  const results = files.map((file) => {
    const raw = fs.readFileSync(path.join(modelsDir, file), 'utf8');
    return validateFile(file, raw, byFile.get(file), options);
  });

  for (const entry of policy.protectedPages) {
    if (!fileSet.has(entry.file)) {
      results.push({
        file: entry.file,
        failures: ['protected page listed in editorial-policy.json but missing from models directory'],
        warnings: [],
        title: null,
        isProtected: true,
      });
    }
  }

  const titleGroups = new Map();
  for (const result of results) {
    if (!result.title) continue;
    const norm = normalizeTitle(result.title);
    if (!titleGroups.has(norm)) titleGroups.set(norm, []);
    titleGroups.get(norm).push(result.file);
  }
  for (const [, filesWithTitle] of titleGroups) {
    if (filesWithTitle.length < 2) continue;
    for (const result of results) {
      if (!filesWithTitle.includes(result.file)) continue;
      const others = filesWithTitle.filter((f) => f !== result.file);
      result.failures.push(`duplicate normalized title, also used by: ${others.join(', ')}`);
    }
  }

  results.sort((a, b) => a.file.localeCompare(b.file));

  const summary = {
    filesChecked: files.length,
    protectedPages: results.filter((r) => r.isProtected).length,
    filesWithFailures: results.filter((r) => r.failures.length > 0).length,
    filesWithWarnings: results.filter((r) => r.warnings.length > 0).length,
    totalFailures: results.reduce((sum, r) => sum + r.failures.length, 0),
    totalWarnings: results.reduce((sum, r) => sum + r.warnings.length, 0),
  };

  return { results, summary };
}

function parseArgs(argv) {
  const options = {
    modelsDir: path.join(__dirname, '..', 'models'),
    policyPath: path.join(__dirname, 'editorial-policy.json'),
    allowIntroFailures: false,
    json: false,
  };
  for (const arg of argv) {
    if (arg === '--allow-intro-failures') {
      options.allowIntroFailures = true;
    } else if (arg === '--json') {
      options.json = true;
    } else if (arg.startsWith('--models-dir=')) {
      options.modelsDir = path.resolve(arg.slice('--models-dir='.length));
    } else if (arg.startsWith('--policy=')) {
      options.policyPath = path.resolve(arg.slice('--policy='.length));
    } else if (arg === '--help' || arg === '-h') {
      options.help = true;
    }
  }
  return options;
}

function printReport(results, summary) {
  for (const result of results) {
    if (result.failures.length === 0 && result.warnings.length === 0) continue;
    console.log(`${result.file}${result.isProtected ? ' [protected]' : ''}`);
    for (const failure of result.failures) console.log(`  FAIL: ${failure}`);
    for (const warning of result.warnings) console.log(`  WARN: ${warning}`);
  }
  console.log('');
  console.log(
    `Checked ${summary.filesChecked} files (${summary.protectedPages} protected): ` +
      `${summary.totalFailures} failure(s) in ${summary.filesWithFailures} file(s), ` +
      `${summary.totalWarnings} warning(s) in ${summary.filesWithWarnings} file(s).`,
  );
}

function main() {
  const options = parseArgs(process.argv.slice(2));
  if (options.help) {
    console.log(
      'Usage: node validate-models.js [--models-dir=DIR] [--policy=FILE] [--allow-intro-failures] [--json]\n\n' +
        '  --allow-intro-failures  Downgrade missing/malformed opening-paragraph\n' +
        '                          failures to warnings (default: strict, they fail).\n' +
        '  --json                  Print the raw results as JSON instead of text.\n',
    );
    return;
  }

  let policy;
  try {
    policy = loadPolicy(options.policyPath);
  } catch (error) {
    console.error(`Could not load editorial policy: ${error.message}`);
    process.exitCode = 1;
    return;
  }

  const { results, summary } = validateCorpus(options.modelsDir, policy, options);

  if (options.json) {
    console.log(JSON.stringify({ results, summary }, null, 2));
  } else {
    printReport(results, summary);
  }

  process.exitCode = summary.totalFailures > 0 ? 1 : 0;
}

if (require.main === module) main();

module.exports = {
  VALID_STATUSES,
  VALID_LANGUAGES,
  VALID_WEBPPL_VERSIONS,
  VALID_CATEGORIES,
  GENERIC_PROSE_PATTERNS,
  parseFrontmatter,
  normalizeTitle,
  computeBodyHash,
  checkOpeningParagraph,
  findGenericProse,
  loadPolicy,
  validateFile,
  validateCorpus,
  parseArgs,
};
