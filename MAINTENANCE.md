# Forest maintenance notes

Status of the forestdb.org cleanup, and what still needs doing. Written
2026-06-12. This file is excluded from the Jekyll build.

## How to verify models

- **WebPPL models** are checked automatically: `cd scripts/test-models &&
  npm install --install-strategy=nested && node runner.js`. The runner runs
  every code box headless against the webppl version the page declares,
  stubbing the browser-only globals (`viz`, `vizPrint`, `print`, `editor`,
  `window`). It writes `report.md`/`report.json`. The monthly
  `test-models` GitHub Action posts the report to a recurring issue.
- **Church models** have no headless runner (webchurch is browser-only). They
  were verified for this pass by loading each page in a local Jekyll +
  Chrome-for-Testing session and clicking every Run button
  (`/tmp/church-check.mjs`, not committed). To re-verify: `docker run --rm -v
  "$PWD":/srv/jekyll -w /srv/jekyll -p 4000:4000 bretfisher/jekyll serve
  --host 0.0.0.0`, then drive the pages with the chrome-cdp skill.

## Current model-status snapshot

After this pass nearly every model carries a verified status. Counts:

- `code`: ~185 (all webppl models that pass the smoke test + all church
  models verified in-browser)
- `code-fail`: 6 (see below)
- `stub`: 6 (empty placeholders, see below)
- `hidden`: 7 (intentionally off the index, e.g. `example.md`)
- `link`: 2 (pages that only point to external code)
- no status: 1 (`bayesian-data-analysis.md`, see below)

## Remaining `code-fail` models (and why)

These are honestly flagged and show a warning banner. Each would be a real
project to fix:

1. **liquid_physics.md** — LiquidFun scripts are now vendored under
   `assets/vendor/liquidfun/` (previously dead links to web.mit.edu). The page
   loads and creates a WebGL canvas, but the 2014 Emscripten build of
   `liquidfun.js` calls `abort(13)` in `new b2World` on modern browsers. Fix =
   rebuild liquidfun.js with a current Emscripten toolchain, or swap to a
   maintained Box2D/LiquidFun WASM port.
2. **newton.md** — written for desktop Church (Bher/Ikarus). Uses internal
   `define`s, `begin`/`display`, and file I/O primitives webchurch lacks, and
   the Runge-Kutta-inside-MH inference is too heavy for the in-browser engine.
   Fix = port to webchurch idioms and a lighter inference setup, or to WebPPL.
3. **inverse-statics.md** — the code is Terra, not Church. Left as-is (the
   page is honest about this). Could be ported to WebPPL.
4. **py-ngram.md** — errors at runtime in webchurch ("Cannot read properties
   of null"). The underlying Pitman-Yor machinery is fine (see the working
   `pymem.md`); the bug is in this page's hierarchical-backoff / markov-memory
   code. Needs interactive webchurch debugging.
5. **ibp.md** — the procedure *deliberately* does not halt (it illustrates
   that the naive IBP stick-breaking program has no computable de Finetti
   representation). The code box is now marked `norun` and the syntax typos
   (smart quotes, a stray `}`) are fixed so it reads correctly as a static
   listing. This one is "correct as-is" — leave it.
6. **hdp-lda.md** — labeled "only a sketch": the first box is missing
   `mh-query` sample args and references undefined `corpus-*` variables. Fix =
   complete the HDP-LDA model (real research work) or mark the sketch boxes
   `norun`.

### Fixed this pass (were code-fail, now `code`)

kalman-filter, curve-fitting, hmm-ising, infinite-dm-mixture, layout, pymem,
plural-predication-webppl, wonkyworlds. Most were ERP-era API calls
(`binomialERP`/`bernoulliERP`/`categoricalERP`, two-arg `.score`), lodash-3
calls (`_.object`/`_.contains`), missing `return`s, `letrec`/`cadr`/`my-pi`
webchurch gaps, or chain-mixing/zero-probability issues. See git log.

## Stubs (6 empty placeholders)

These have only frontmatter, no content. They are honest `stub`s (asterisk
badge, not links). Filling them is *new* content, not a fix:

- `little-trees.md` — Concept Learning. A real source exists
  (github.com/stuhlmueller/little-trees: a noisy tree-grammar concept-learning
  model). Could be ported to a runnable WebPPL/Church model.
- `bn-structure.md` — Bayes-net structure learning.
- `semantic-free-vars.md` — pragmatics with semantic free variables.
- `genetic-linkage.md`, `network-analysis.md`, `seismic-events.md` — PPAML
  challenge-problem placeholders; these are genuinely hard benchmark models.

Recommendation: either write faithful runnable models for these or convert
them to `link`s pointing at an existing implementation. Don't fabricate.

## Models needing a status decision

- **bayesian-data-analysis.md** — a 23-box BDA tutorial. Individual boxes are
  exposition fragments that reference variables defined in earlier boxes, so
  they error when run standalone (same pattern the webppl class projects had).
  It needs a per-box pass marking the fragment boxes `norun` and confirming
  the self-contained boxes run; then mark `code`. Left no-status for now.

## Prose / content quality (the big remaining task)

Only a light typo pass was done (`embedded-counterfactuals.md`,
`the-49ers-are-going-to-win.md`). A thorough prose pass across all ~200 pages
is still wanted:

- Spelling/grammar (several class-project pages are rough).
- Broken or bare markdown, stray prose pasted inside code fences (a few were
  fixed as code bugs, e.g. `false-cognates.md`).
- Many class-project titles are author names ("Jin, Mai, Saavedra, Syracuse -
  Irony") rather than descriptive; consider a title-normalization pass while
  preserving attribution in the page body.
- Verify `cite:`/`ref:` bibliography keys still resolve against
  `bibliography.bib`.

Approach: fan out per-page with subagents, preserve author voice, never touch
code. Keep diffs prose-only so the smoke test stays green.

## UI/UX improvements (requested)

- **Intro / landing**: the index intro is a plain paragraph. Improve the
  first-impression UI — clearer hero, what Forest is, how to run a model, the
  language badges legend (the green check / orange X / asterisk are currently
  unexplained), maybe counts per category and a prominent search.
- **Badges legend**: add a small key explaining the status icons.
- **Search**: the box uses fuzzy typeahead now; consider full-text search over
  model prose (e.g. lunr.js) instead of title-only.
- **Tech stack**: Bootstrap 3.1.1 / jQuery 1.11 are from 2014. A modern
  restyle is optional and only worth it if Forest is being actively reinvested
  in (see the strategic note below).

## Known harness limitations

- 5 boxes across `elephants.md` / `elephants_continuized.md` chain state
  through the browser-only `wpEditor` (`editor.put`/`editor.get` of *function*
  values across boxes). They run on the live site but can't be reconstructed
  headless; the runner classifies them "browser-only", not failures.
- `adj-order-appendix.md` box 8 is a heavy inference that times out headless
  at 120s but runs in the browser (verified) — marked `code`.

## Strategic note

The deeper question (raised in the original review): is Forest a living
teaching resource, a historical archive, or worth reinvesting in? The cheap,
high-value direction is to keep it healthy as teaching infrastructure (CI is
now in place) and lean into the machine-readable `models.json` corpus as a
citable dataset of ~200 human-annotated generative models — useful for
LLM-era probabilistic-program-synthesis evals.
