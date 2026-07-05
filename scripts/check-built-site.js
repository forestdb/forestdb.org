#!/usr/bin/env node
'use strict';

const fs = require('fs');
const path = require('path');

function occurrences(haystack, needle) {
  if (!needle) return 0;
  return haystack.split(needle).length - 1;
}

function plural(count) {
  return count === 1 ? 'time' : 'times';
}

function indexProblems(models, html) {
  const problems = [];
  for (const model of models) {
    const modelPath = new URL(model.url).pathname;
    const count = occurrences(html, `href="${modelPath}"`);
    const expected = model.status === 'hidden' ? 0 : 1;
    if (count !== expected) {
      problems.push(`${model.title} appears ${count} ${plural(count)} on the index; expected ${expected}`);
    }
  }
  return problems;
}

function htmlFiles(dir) {
  const files = [];
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) files.push(...htmlFiles(full));
    else if (entry.isFile() && entry.name.endsWith('.html')) files.push(full);
  }
  return files;
}

function localTarget(siteDir, sourceFile, rawUrl) {
  if (!rawUrl || rawUrl.startsWith('#') || rawUrl.startsWith('//')) return null;
  if (/^(?:[a-z]+:)/i.test(rawUrl)) return null;
  const withoutSuffix = rawUrl.split(/[?#]/, 1)[0];
  if (!withoutSuffix) return null;
  let decoded;
  try {
    decoded = decodeURI(withoutSuffix);
  } catch (error) {
    if (error instanceof URIError) return path.join(siteDir, '__invalid_uri__');
    throw error;
  }
  const resolved = decoded.startsWith('/')
    ? path.join(siteDir, decoded.slice(1))
    : path.resolve(path.dirname(sourceFile), decoded);
  if (decoded.endsWith('/') || (fs.existsSync(resolved) && fs.statSync(resolved).isDirectory())) {
    return path.join(resolved, 'index.html');
  }
  return resolved;
}

function internalLinkProblems(siteDir) {
  const problems = [];
  const indexFile = path.join(siteDir, 'index.html');
  const modelsDir = path.join(siteDir, 'models');
  const sourceFiles = [];
  if (fs.existsSync(indexFile)) sourceFiles.push(indexFile);
  if (fs.existsSync(modelsDir)) sourceFiles.push(...htmlFiles(modelsDir));
  for (const sourceFile of sourceFiles) {
    const html = fs.readFileSync(sourceFile, 'utf8');
    const urls = [...html.matchAll(/\b(?:href|src)=["']([^"']+)["']/gi)].map((match) => match[1]);
    for (const url of urls) {
      const target = localTarget(siteDir, sourceFile, url);
      if (target && !fs.existsSync(target)) {
        const source = path.relative(siteDir, sourceFile);
        problems.push(`${source} links to missing target ${url}`);
      }
    }
  }
  return [...new Set(problems)].sort();
}

function main() {
  const siteDir = path.resolve(process.argv[2] || path.join(__dirname, '..', '_site'));
  const modelIndexPath = path.join(siteDir, 'models.json');
  const htmlPath = path.join(siteDir, 'index.html');
  let payload;
  let html;
  try {
    payload = JSON.parse(fs.readFileSync(modelIndexPath, 'utf8'));
    html = fs.readFileSync(htmlPath, 'utf8');
  } catch (error) {
    if (error.code === 'ENOENT' || error instanceof SyntaxError) {
      console.error(`Could not read built site in ${siteDir}`);
      process.exitCode = 1;
      return;
    }
    throw error;
  }

  if (!payload || !Array.isArray(payload.models) || payload.count !== payload.models.length) {
    console.error('Built models.json has an invalid count or models array.');
    process.exitCode = 1;
    return;
  }

  const problems = indexProblems(payload.models, html).concat(internalLinkProblems(siteDir));
  if (problems.length > 0) {
    console.error(problems.join('\n'));
    process.exitCode = 1;
    return;
  }

  const visible = payload.models.filter((model) => model.status !== 'hidden').length;
  console.log(`Built site lists ${visible} visible models exactly once, omits hidden models, and has no broken internal links.`);
}

if (require.main === module) main();

module.exports = { indexProblems, internalLinkProblems };
