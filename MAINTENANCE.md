# Forest maintenance notes

Status of the forestdb.org cleanup, and what still needs doing. Written
2026-06-12; updated later the same day after a second pass that resolved
every open item from the first version of this file. This file is excluded
from the Jekyll build.

## How to verify models

- **WebPPL models** are checked automatically: `cd scripts/test-models &&
  npm install --install-strategy=nested && node runner.js`. The runner runs
  every code box headless against the webppl version the page declares,
  stubbing the browser-only globals (`viz`, `vizPrint`, `print`, `editor`,
  `window`). It writes `report.md`/`report.json`. The monthly
  `test-models` GitHub Action posts the report to a recurring issue.
  Boxes that depend on browser-only state (wpEditor chaining, WebGL/LiquidFun
  `Testbed`/`b2World`) are classified "browser-only", not failures.
- **Church models** can now be run headless, despite webchurch being
  nominally browser-only: clone github.com/probmods/webchurch, `git
  submodule update --init`, then `npm install esprima@1.0.4
  escodegen@0.0.26 underscore@1.6.0 source-map@0.1.30 seedrandom
  --ignore-scripts` and call `evaluate.js` from a small node script
  (the `church` CLI wrapper needs its `require('sys')` bypassed; inject
  JS-level stubs for `barplot`/`hist`/`scatter`). This was used to verify
  bayesian-data-analysis (17 boxes), py-ngram (600+ seeded runs), prosody,
  and the hdp-lda sketches. In-browser verification still works too:
  `docker run --rm -v "$PWD":/srv/jekyll -w /srv/jekyll -p 4000:4000
  bretfisher/jekyll serve --host 0.0.0.0`, then drive pages with the
  chrome-cdp skill.

## Current model-status snapshot

Every model carries a status. Counts (214 pages):

- `code`: 197. WebPPL models pass the headless smoke test; Church models
  verified in-browser or via the headless webchurch harness
- `static`: 3 — intentional static listings (see below). This status
  replaced `code-fail` once no genuinely broken pages remained; the index
  badge is a neutral gray listing icon, and the page banner says "static
  listing", not "may fail".
- `link`: 6 — pages that point to external implementations
- `hidden`: 8 — intentionally off the index, e.g. `example.md`;
  also `little-trees.md` (see below)
- no status: 0. The `stub` status was removed entirely (index template,
  legend, search.json, README) once the last stubs were resolved.

Primary category counts: Language and Pragmatics 98; Graphical Models and
Causality 25; Probability and Bayesian Data Analysis 20; Bayesian
Nonparametrics 19; Agents, Games, and Social Reasoning 16; Regression and
Statistical Learning 11; Time Series and Stochastic Processes 10; Scientific
and Physical Models 9; Program Induction and Concept Learning 6.

## `static` pages (all by design)

All three render their code as `norun` static listings with an explanatory
note and an accurate `model-status-verbose`; nothing on these pages
pretends to run:

1. **ibp.md** — the procedure deliberately does not halt (illustrates that
   the naive IBP stick-breaking program has no computable de Finetti
   representation). Correct as-is.
2. **hdp-lda.md** — both boxes are sketches (missing `mh-query` args,
   undefined `corpus-*` vars; the second targets the shred2014 Church
   dialect). Headless webchurch runs confirmed neither can execute.
   Completing the model would be real research work.
3. **inverse-statics.md** — the code is Terra, not Church. Could be ported
   to WebPPL someday; until then it is a static listing.

### Fixed in the second pass (were failing or unresolved)

- **newton.md** — ported from desktop Church (Bher/Ikarus) to WebPPL:
  same force library, RK4, and soft-Gaussian conditioning; lighter MH setup.
  Passes the runner in ~19s; recovers elasticity (E≈0.11 vs true 0.1) and
  detects collision forces correctly on both test scenes. Original Church
  code preserved as a labeled `norun` listing.
- **py-ngram.md** — root-caused via the headless webchurch harness. The
  "Cannot read properties of null" was webchurch's error reporter choking on
  the real error: stack overflow in `pick-a-stick` when the unbounded
  per-prefix discount hyperprior sampled large `a`. Fix: bound the discount
  prior to [0, 0.3] (commented in code), plus a real backoff bug — `rest`
  dropped the most recent word instead of the oldest (`but-last`). 0
  failures across 600+ seeded runs.
- **liquid_physics.md** — the 2014 MIT-fork build of `liquidfun.js`
  required an external `lf_core.js.mem` memory initializer that was never
  vendored, so static data stayed zeroed and `new b2World` hit `abort(13)`.
  Replaced with the official google/liquidfun 1.1.0 testbed build
  (self-contained, same API; provenance banner in the file). Verified in
  headless Chrome: ~360 particles simulate and settle, re-run path clean.
- **prosody.md** — the "model times out" claim was stale: the unmodified
  program completes in ~10.6s headless (pure `enumeration-query`,
  deterministic). Output matches the Bergen 2014 prosody story. Marked
  `code` with a runtime warning line.
- **bayesian-data-analysis.md** — per-box pass over all 23 boxes: 6
  fragment/pseudo-code boxes marked `norun`; the 17 self-contained boxes
  all execute headless (heaviest: 5s). Marked `code`.

## Former stubs (all resolved)

- `bn-structure.md` — now a runnable WebPPL model (new content): exact
  posterior over the 25 three-variable DAGs via enumeration with
  Cooper–Herskovits marginal likelihood; recovers the Markov equivalence
  class of the data-generating chain. Passes the runner.
- `little-trees.md` — the only existing implementation
  (github.com/stuhlmueller/little-trees) is a private repo, so the page is
  `hidden` for now, with a short description and no link. If the repo goes
  public: re-add the link and set `model-status: link`.
- `semantic-free-vars.md` — `link` to three problang.org chapters
  (vagueness thresholds, scope ambiguity, lexical uncertainty).
- `genetic-linkage.md` — `link` to SUPERLINK / Fishelson & Geiger
  resources. Research note: genetic linkage is *not* an official PPAML
  challenge problem (the archived Galois wiki lists CP1–CP10; it's not
  there); the category was kept but the body doesn't claim a CP number.
- `network-analysis.md` — identified as PPAML CP4 Small Problem 6
  ("Network Analysis Expressiveness Challenge", preferential-attachment
  mixture); `link` to the surviving fork's problem spec PDF and the
  archived CP4 wiki. No reference solution exists anywhere.
- `seismic-events.md` — identified as the NET-VISA / CP4 signal
  interpretation problem; `link` to the BSSA 2013 paper, the archived
  problem statement, and nimar/seismic-2d.

## Prose / content quality (done)

A full prose pass ran across all ~200 pages (28 parallel subagents,
~350 fixes: spelling, grammar, broken markdown, malformed links). Code
boxes, frontmatter, and citation keys were off-limits; a mechanical
invariance check confirmed the pass changed zero code bytes and zero
frontmatter. Also done:

- **Title normalization**: 31 class-project pages renamed from author-name
  lists ("Jin, Mai, Saavedra, Syracuse - Irony") to descriptive titles
  derived from page content; attribution preserved/added as a "*By ...*"
  line in each body. Titles verified unique site-wide; URLs unchanged.
- **Bibliography**: every `Cite:`/`Ref:` key cross-checked against
  `bibliography.bib` (case-insensitive, matching the parse-bibtex.js
  rules). One missing entry added (`Bass2015NotBN`, verified against the
  paper PDF). All 49 keys resolve; no orphan bib entries.
- **2025-problang-metaphor.md**: ~100 lines of model code were rendering
  as raw prose because a fence never reopened; now a proper runnable box
  (passes the runner).

### Author-owned semantic questions

These pages run, but changing their interpretation requires an author-level
decision. Do not guess at the intended semantics:

- `intervention-counterfactuals.md` discusses `(and smokes cold)`, while
  `smokes` is not defined in the model.
- `because.md` leaves the intended `eps` polarity unclear.
- `habituals-cogsci2016.md` may swap the prose descriptions of its scale
  parameters.
- `2025-problang-teasing.md` describes `phi` with the opposite polarity from
  its setup.
- `elephants.md` and `elephants_continuized.md` disagree about the `S2` scope
  label.
- `lxz-chinese-scope.md` has ambiguous conclusions phrased as "not
  unavailable."

### Editorial policy and validation

- `scripts/editorial-policy.json` records protected third-party and
  paper-companion pages. Its body hashes permit site-owned frontmatter changes
  while rejecting unreviewed body edits.
- `scripts/validate-models.js` enforces the nine-category taxonomy, statuses,
  unique titles, protected hashes, and concise introductions on maintained
  pages.
- `scripts/test-models/assert-report.js` turns the WebPPL smoke-test report into
  a gate, allowing only the documented `adj-order-appendix.md` box 8 timeout.
- `scripts/check-built-site.js` verifies that every visible model appears once
  on the rendered index, every hidden model appears zero times, and model pages
  have no broken internal links.

## UI/UX (done)

- **Hero**: index.md opens with a jumbotron — what Forest is, live model
  count (Liquid-computed), how to run a model, contribute + models.json
  buttons. Per-category count badges on each section header.
- **Badges legend**: a key under the page header explains the status
  icons (check = runs, bookmark = external link, gray listing icon =
  static listing).
- **Full-text search**: lunr.js 2.3.9 vendored under `assets/vendor/lunr/`;
  `search.json` (Liquid template, like models.json) indexes title, tags,
  category, and page text including code. The navbar typeahead lazy-loads
  the index on first interaction; ranking is exact-title > fuzzy-title >
  prose match. Falls back to title-only before the index loads, and the
  server-rendered list works with JS off. Browser-verified (prose-only
  query "coffee" returns 6 results; click-through navigates).
- **Tech stack**: still Bootstrap 3.1.1 / jQuery 1.11. A modern restyle
  remains optional and only worth it if Forest gets active reinvestment
  (see the strategic note).

## Known harness limitations

- 5 boxes across `elephants.md` / `elephants_continuized.md` chain state
  through the browser-only `wpEditor` (`editor.put`/`editor.get` of
  *function* values across boxes). They run on the live site; the runner
  classifies them "browser-only".
- `liquid_physics.md` needs real browser WebGL globals; the runner now
  classifies it "browser-only" (it is browser-verified working).
- `adj-order-appendix.md` box 8 is a heavy stochastic inference that can
  exceed the 120-second headless limit, although it also completes within the
  limit on many runs and works in the browser. The report assertion permits
  only this specific timeout.

## Strategic note

The deeper question (raised in the original review): is Forest a living
teaching resource, a historical archive, or worth reinvesting in? The cheap,
high-value direction is to keep it healthy as teaching infrastructure (CI is
now in place) and lean into the machine-readable `models.json` corpus as a
citable dataset of 214 human-annotated generative models, useful for
LLM-era probabilistic-program-synthesis evals.
