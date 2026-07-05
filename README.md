Forest
======

Forest ([forestdb.org](https://forestdb.org)) is a community repository for
generative models written in probabilistic programming languages, mainly
[Church](https://github.com/probmods/webchurch) and
[WebPPL](https://github.com/probmods/webppl). Most code boxes are editable and
run directly in the browser. Forest is maintained as a teaching resource and
archive; it has long served as the home for class projects from courses such
as [Probabilistic Languages Understanding](https://problang.org).

Adding models
-------------

To add a new model, create a markdown file in the
[models directory](https://github.com/forestdb/forestdb.org/tree/gh-pages/models).
You can do this directly on GitHub: [add model](https://github.com/forestdb/forestdb.org/new/gh-pages/models).

For an example of the file format, see
[example.md](https://raw.githubusercontent.com/forestdb/forestdb.org/gh-pages/models/example.md).
A minimal model file looks like this:

    ---
    layout: model
    title: My Model
    model-language: webppl
    model-status: code
    model-category: Program Induction and Concept Learning
    model-tags: concepts, program induction
    ---

    Prose describing the model.

    ~~~~
    flip(0.5)
    ~~~~

Frontmatter fields:

- `model-language` (required to make code boxes runnable): `webppl` or `church`.
- `model-language-version` (optional, webppl only): pins a webppl version.
  Available: `pre-v0.7`, `v0.9.6`, `v0.9.7`, `v0.9.9`, `v0.9.13`, `v0.9.15`
  (default).
- `model-status` (optional): `code` (code runs), `static` (code shown as a
  non-runnable listing; add a `model-status-verbose` explanation), `link`
  (page links to external code), or `hidden` (not listed on the front page).
- `model-category` (required): one of
  `Probability and Bayesian Data Analysis`, `Graphical Models and Causality`,
  `Regression and Statistical Learning`, `Time Series and Stochastic Processes`,
  `Bayesian Nonparametrics`, `Program Induction and Concept Learning`,
  `Language and Pragmatics`, `Agents, Games, and Social Reasoning`, or
  `Scientific and Physical Models`. Use tags for cross-cutting provenance and
  applications such as `benchmark`, `PPAML`, `replication`, or a scientific
  domain.
- `model-tags` (optional): arbitrary comma-separated words or phrases.

Choose the primary category by the model's teaching target. Language meaning
and use take precedence over recursive-agent machinery. Nonlinguistic choice,
teaching, games, and social reasoning belong under agents. Inferred programs,
rules, grammars, and concepts belong under program induction. Use Bayesian
nonparametrics when the nonparametric prior itself is the lesson; use time
series for temporal latent states or event streams; use graphical models for
factorization, interventions, or causality; and use scientific models when a
specific domain mechanism is central. General inference and hierarchical Bayes
belong under probability and Bayesian data analysis.

To display a code box without making it runnable (e.g. to show a model
fragment), mark the fence with `norun`:

    ~~~~ norun
    // this box renders as static code
    ~~~~

Model coverage
--------------

Forest contains 214 models spanning Bayesian data analysis, graphical and
causal models, statistical learning, time series, Bayesian nonparametrics,
program induction, language and pragmatics, agent reasoning, and scientific
models. The collection includes drift diffusion, item-response theory,
survival with censoring, phylogenetics, Hawkes processes, stochastic
volatility, interventional structure learning, partial pooling, signal
detection, and Gaussian-process regression alongside the original Church and
WebPPL teaching corpus.

Machine-readable index
----------------------

[forestdb.org/models.json](https://forestdb.org/models.json) lists every model
with title, URL, markdown source URL, language, version, status, category, and
tags.

Local development
-----------------

The site is plain [Jekyll](https://jekyllrb.com/), built by GitHub Pages from
the `gh-pages` branch. To preview locally:

    docker run --rm -v "$PWD":/site -p 4000:4000 -w /site jekyll/jekyll:4.4.1 \
      jekyll serve --host 0.0.0.0

then open [localhost:4000](http://localhost:4000).

All JavaScript and CSS (jQuery, Bootstrap, the webppl runtimes, webchurch) is
vendored under `assets/`, so the site has no runtime dependencies on external
CDNs. To support a new webppl version, build or download the browser bundle,
add it as `assets/webppl/webppl-<version>.js`, and add the version to the list
in this README and to `scripts/test-models/package.json`.

Continuous checks
-----------------

Automated checks keep the site healthy (see `.github/workflows/`):

- **Validate models** checks frontmatter, category membership, unique titles,
  concise introductions on repository-maintained pages, and body hashes for
  protected third-party pages. Run `node scripts/validate-models.js` locally.
- **Test models** runs every WebPPL model headless against its declared
  version and rejects failures outside the documented baseline. Run locally
  with `cd scripts/test-models && npm install --install-strategy=nested &&
  npm run check` (the nested install is needed for WebPPL 0.6.1's hardcoded
  module paths).
- **Check links** runs lychee over all model pages and files a report issue
  when links break.

License
-------

Site code is MIT-licensed; models belong to their authors, with new
contributions accepted under CC-BY 4.0 (prose) and MIT (code). See
[LICENSE.md](LICENSE.md).
