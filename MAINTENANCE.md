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

Every model now carries a status. Counts (204 pages):

- `code`: 187 — webppl models pass the headless smoke test; church models
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

### Follow-up queue: in-code issues flagged during the prose pass

The prose agents were barred from touching code; these flags are recorded
for a future code pass (verify before fixing — some may be intentional):

- `blm.md` — Model 2 `utterancePrior` is missing a `return` before
  `uniformDraw(...)` (likely real bug).
- `logistic-regression.md` — `(flip (sigmoid x) label)` passes an extra
  arg; probably meant `(flip (sigmoid x))`.
- `exhaustivity.md` — Scheme `case` clauses like `((utterance) 0.99)` match
  the literal symbol, not the variable's value.
- `cushman-generics.md` — first standalone `pragmaticListener` box returns
  `sig`, which is undefined in that snippet.
- `2025-problang-irony.md` — `statePrior` weights are reversed between the
  intro box and the later boxes (contradicting the prose); `amazingeDist`
  typo breaks the swap the prose suggests.
- `intervention-counterfactuals.md` — prose discusses `(and smokes cold)`
  but `smokes` is commented out of the utterance prior and never defined.
- Stray `///` fold-close markers with no `///fold:` opener in
  `gl-polite-irony.md`, `questions-answers.md`, `adj-order-appendix.md`,
  `generic-id.md`, `torabian-politeness-QUDs.md`.
- `infinite-hmm.md` — second version's `transition` returns the transition
  model instead of sampling from it.
- `ncrp-hdp.md` — third `hist` labeled "Root Category" but samples
  `sample-observation`.
- Copy-paste comment rot: several sarcasm/hyperbole-family pages
  (`sarcasm_tone1/2.md`, `sarcasm_cg1.md`, `spokenIrony.md`,
  `hyperbole-distance-L1/L2.md`) carry "price state" comments from the
  watch-price model they were cloned from.
- Semantic prose/code mismatches needing an author-level decision:
  `because.md` (eps polarity), `habituals-cogsci2016.md` (sigma description
  likely swapped), `2025-problang-teasing.md` (phi polarity contradicts its
  own setup), `elephants.md` vs `elephants_continuized.md` (S2 scope label),
  `lxz-chinese-scope.md` conclusions ("not unavailable").

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
- `adj-order-appendix.md` box 8 is a heavy inference that times out
  headless at 120s but runs in the browser (verified) — marked `code`.
  This is the only failing entry in the headless report.

## Strategic note

The deeper question (raised in the original review): is Forest a living
teaching resource, a historical archive, or worth reinvesting in? The cheap,
high-value direction is to keep it healthy as teaching infrastructure (CI is
now in place) and lean into the machine-readable `models.json` corpus as a
citable dataset of ~200 human-annotated generative models — useful for
LLM-era probabilistic-program-synthesis evals.
