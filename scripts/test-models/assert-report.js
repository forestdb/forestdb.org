#!/usr/bin/env node
'use strict';

const fs = require('fs');
const path = require('path');

const BASELINE_FAILURES = [
  {
    filename: 'adj-order-appendix.md',
    box: 8,
    timedOut: true,
  },
];

const ALLOWED_SKIPS = {
  'plural-predication.md': 'no runnable code boxes',
  'semantic-free-vars.md': 'no runnable code boxes',
};

function isBaselineFailure(filename, failure) {
  return BASELINE_FAILURES.some((expected) =>
    expected.filename === filename &&
    expected.box === failure.box &&
    expected.timedOut === Boolean(failure.timedOut));
}

function unexpectedFailures(results) {
  return results.filter((result) => {
    const failures = result.failures || [];
    if (failures.length === 0) return false;
    return !failures.every((failure) => isBaselineFailure(result.filename, failure));
  });
}

function reportProblems(results, expectedFiles) {
  const problems = [];
  if (results.length === 0) problems.push('report is empty');
  const counts = new Map();
  for (const result of results) counts.set(result.filename, (counts.get(result.filename) || 0) + 1);
  for (const [filename, count] of counts) {
    if (count > 1) problems.push(`duplicate result for ${filename}`);
  }
  const expected = new Set(expectedFiles);
  for (const filename of expectedFiles) {
    if (!counts.has(filename)) problems.push(`missing result for ${filename}`);
  }
  for (const result of results) {
    if (!expected.has(result.filename)) problems.push(`unexpected result for ${result.filename}`);
    if (result.skipped && ALLOWED_SKIPS[result.filename] !== result.skipped) {
      problems.push(`${result.filename} was skipped: ${result.skipped}`);
    }
  }
  return problems;
}

function expectedWebpplFiles(modelsDir) {
  return fs.readdirSync(modelsDir)
    .filter((filename) => filename.endsWith('.md'))
    .filter((filename) => {
      const text = fs.readFileSync(path.join(modelsDir, filename), 'utf8');
      return /^model-language:\s*webppl\s*$/m.test(text);
    })
    .sort();
}

function main() {
  const reportPath = path.resolve(process.argv[2] || path.join(__dirname, 'report.json'));
  let results;
  try {
    results = JSON.parse(fs.readFileSync(reportPath, 'utf8'));
  } catch (error) {
    if (error.code === 'ENOENT' || error instanceof SyntaxError) {
      console.error(`Could not read model report: ${reportPath}`);
      process.exitCode = 1;
      return;
    }
    throw error;
  }

  if (!Array.isArray(results)) {
    console.error(`Model report is not an array: ${reportPath}`);
    process.exitCode = 1;
    return;
  }

  const modelsDir = path.resolve(__dirname, '..', '..', 'models');
  const structuralProblems = reportProblems(results, expectedWebpplFiles(modelsDir));
  const unexpected = unexpectedFailures(results);
  if (structuralProblems.length === 0 && unexpected.length === 0) {
    console.log('Model report is complete and has no unexpected failures.');
    return;
  }

  for (const problem of structuralProblems) console.error(`- ${problem}`);
  if (unexpected.length > 0) console.error(`${unexpected.length} model(s) have unexpected failures:`);
  for (const result of unexpected) {
    console.error(`- ${result.filename}`);
    for (const failure of result.failures || []) {
      console.error(`  box ${failure.box}: ${failure.error}`);
    }
  }
  process.exitCode = 1;
}

if (require.main === module) main();

module.exports = { ALLOWED_SKIPS, BASELINE_FAILURES, reportProblems, unexpectedFailures };
