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
    model-category: Probabilistic Language Understanding
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
- `model-status` (optional): `code` (code runs), `code-fail` (known broken;
  add a `model-status-verbose` explanation), `link` (page links to external
  code), `stub`, or `hidden` (not listed on the front page).
- `model-category` (optional but encouraged): one of
  `Concept Learning`, `Reasoning about Reasoning`,
  `Probabilistic Language Understanding`, `Counterfactuals and Explanations`,
  `Machine Learning`, `Nonparametric Models`, `Bayesian Data Analysis`,
  `Undirected Constraints`, `Inverse Dynamics`, `PPAML Challenge Problems`,
  `Miscellaneous`. Models without a category are listed under "Uncategorized"
  on the front page.
- `model-tags` (optional): arbitrary comma-separated words or phrases.

To display a code box without making it runnable (e.g. to show a model
fragment), mark the fence with `norun`:

    ~~~~ norun
    // this box renders as static code
    ~~~~

Machine-readable index
----------------------

[forestdb.org/models.json](https://forestdb.org/models.json) lists every model
with title, URL, markdown source URL, language, version, status, category, and
tags.

Local development
-----------------

The site is plain [Jekyll](https://jekyllrb.com/), built by GitHub Pages from
the `gh-pages` branch. To preview locally:

    docker run --rm -v "$PWD":/site -p 4000:4000 -w /site jekyll/jekyll \
      jekyll serve --host 0.0.0.0

then open [localhost:4000](http://localhost:4000).

All JavaScript and CSS (jQuery, Bootstrap, the webppl runtimes, webchurch) is
vendored under `assets/`, so the site has no runtime dependencies on external
CDNs. To support a new webppl version, build or download the browser bundle,
add it as `assets/webppl/webppl-<version>.js`, and add the version to the list
in this README and to `scripts/test-models/package.json`.

Continuous checks
-----------------

Two scheduled GitHub Actions keep the site healthy (see `.github/workflows/`):

- **Test models** runs every webppl model headless against its declared
  webppl version (`scripts/test-models/runner.js`) and files a report issue.
  Run locally with `cd scripts/test-models &&
  npm install --install-strategy=nested && node runner.js`
  (nested install needed for webppl 0.6.1's hardcoded module paths).
- **Check links** runs lychee over all model pages and files a report issue
  when links break.

License
-------

Site code is MIT-licensed; models belong to their authors, with new
contributions accepted under CC-BY 4.0 (prose) and MIT (code). See
[LICENSE.md](LICENSE.md).
