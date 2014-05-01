Forest
======

Forest is a collaborative repository for generative models.

To add a new model to Forest, simply create a markdown file in the [models directory](https://github.com/forestdb/forestdb.org/tree/gh-pages/models). You can do this directly on github by following this link: [Add model!](https://github.com/forestdb/forestdb.org/new/gh-pages/models)

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
