Forest
======

Forest is a collaborative repository for generative models.

Adding models
-------------

To add a new model to Forest, simply create a markdown file in the [models directory](https://github.com/forestdb/forestdb.org/tree/gh-pages/models). You can do this directly on github by following this link: [add model](https://github.com/forestdb/forestdb.org/new/gh-pages/models).

For examples of the model file format, see:
- [example.md](https://raw.githubusercontent.com/forestdb/forestdb.org/gh-pages/models/example.md) is a model that *is not* shown on the front page.
- [arithmetic.md](https://raw.githubusercontent.com/forestdb/forestdb.org/gh-pages/models/arithmetic.md) is a model that *is* shown on the front page.

For models that are shown on the front page, use additional header entries:

    model-status: code
    model-category: Concept Learning
    model-tags: concepts, program induction

Currently, the following status codes are available:

    code
    link
    code-fail
    stub

The following categories are available:

    Concept Learning
    Reasoning about Reasoning
    Machine Learning
    Nonparametric Models
    Undirected Constraints
    Inverse Dynamics
    PPAML Challenge Problems
    Miscellaneous

Tags can be arbitrary words or phrases separated by commas.

Updating webchurch
------------------

Forest uses [webchurch](https://github.com/probmods/webchurch) to run probabilistic programs. To update webchurch, follow these steps:

1. Clone and build the most recent version of the webchurch repository, following the instructions [here](https://github.com/probmods/webchurch):

    git clone https://github.com/probmods/webchurch.git
    cd webchurch
    git init (only if you are on Windows)
    git submodule update --init --recursive
    npm install
    ./compile.sh

2. Clone the most recent version of the Forest repository:

    git clone https://github.com/forestdb/forestdb.org.git

3. Copy the files in `webchurch/online/` to `forestdb.org/assets/webchurch/`.

4. Run a local webserver in the Forest directory and go to [0.0.0.0:4000](http://0.0.0.0:4000) to check that the changes did not break functionality:

    jekyll serve --watch

5. If code boxes run as expected, great! Commit and push. If not, you may need to update `assets/js/custom.js` in the Forest directory.
