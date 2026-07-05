---
layout: default
title: Forest - A Repository for Generative Models
isindex: true
all-model-statuses: [code, link, static]
all-model-categories:
  - "Probability and Bayesian Data Analysis"
  - "Graphical Models and Causality"
  - "Regression and Statistical Learning"
  - "Time Series and Stochastic Processes"
  - "Bayesian Nonparametrics"
  - "Program Induction and Concept Learning"
  - "Language and Pragmatics"
  - "Agents, Games, and Social Reasoning"
  - "Scientific and Physical Models"
---

{% assign all_models = site.pages | where: "layout", "model" %}
{% assign visible_models = all_models | where_exp: "p", "p.model-status != 'hidden'" %}

<div class="index-hero">
  <p>
    Forest is a community repository of {{ visible_models | size }} generative
    models written in probabilistic programming languages, mainly
    <a href="https://github.com/probmods/webchurch">Church</a> and
    <a href="https://github.com/probmods/webppl">WebPPL</a>, maintained as a
    teaching resource and archive.
  </p>
  <p>
    Most models run right here in your browser: open a model below, edit the
    code box if you like, and press <strong>run</strong>.
  </p>
  <p class="index-hero-actions">
    <a class="btn btn-sm btn-success" href="https://github.com/forestdb/forestdb.org#adding-models">Contribute a model</a>
    <a class="btn btn-sm btn-default" href="/models.json">Machine-readable index</a>
  </p>
</div>

<div class="page-header">
  <h1>Models</h1>
</div>

<p class="model-legend text-muted">
  <span class="legend-item">
    <span class="label label-success glyph-label"><span class="glyphicon glyphicon-ok"></span></span>
    runs in your browser
  </span>
  <span class="legend-item">
    <span class="label label-default glyph-label"><span class="glyphicon glyphicon-bookmark"></span></span>
    links to external code
  </span>
  <span class="legend-item">
    <span class="label label-default glyph-label"><span class="glyphicon glyphicon-align-left"></span></span>
    static code listing (not runnable)
  </span>
</p>

{% for category in page.all-model-categories %}

{% assign category_models = visible_models | where: "model-category", category %}

<div class="list-group">

  <div class="list-group-item" style="background-color: #F9F9F9">
    {{ category }}
    <span class="badge pull-right category-count">{{ category_models | size }}</span>
  </div>

    {% for status in page.all-model-statuses %}
      {% for p in site.pages %}
        {% if p.layout == 'model' %}
          {% if p.model-status == status %}
           {% if p.model-category == category %}
              <div class="list-group-item">
                  <a href="{{ p.url }}">{{ p.title }}</a>
                  {% if p.model-status == 'code' %}
                    <span class="label label-success pull-right glyph-label">
                        <span class="glyphicon glyphicon-ok" rel="tooltip" title="Code runs"></span>
                    </span>
                  {% elsif p.model-status == 'link' %}
                    <span class="label label-default pull-right glyph-label">
                        <span class="glyphicon glyphicon-bookmark" rel="tooltip" title="Link to code"></span>
                    </span>
                  {% elsif p.model-status == 'static' %}
                    <span class="label label-default pull-right glyph-label">
                        <span class="glyphicon glyphicon-align-left" rel="tooltip" title="Static listing ({{ p.model-status-verbose }})"></span>
                    </span>
                  {% endif %}
              </div>
            {% endif %}
          {% endif %}
        {% endif %}
      {% endfor %}
    {% endfor %}

    {% comment %} Models in this category that don't declare a status. {% endcomment %}
    {% for p in site.pages %}
      {% if p.layout == 'model' %}
        {% unless p.model-status %}
          {% if p.model-category == category %}
            <div class="list-group-item">
              <a href="{{ p.url }}">{{ p.title }}</a>
            </div>
          {% endif %}
        {% endunless %}
      {% endif %}
    {% endfor %}

</div>

{% endfor %}

{% comment %} Catch-all so that models without a category are never invisible. {% endcomment %}
{% assign has_uncategorized = false %}
{% for p in site.pages %}
  {% if p.layout == 'model' %}
    {% unless p.model-category %}
      {% if p.model-status != 'hidden' %}
        {% assign has_uncategorized = true %}
      {% endif %}
    {% endunless %}
  {% endif %}
{% endfor %}

{% if has_uncategorized %}
<div class="list-group">

  <div class="list-group-item" style="background-color: #F9F9F9">
    Uncategorized
  </div>

  {% for p in site.pages %}
    {% if p.layout == 'model' %}
      {% unless p.model-category %}
        {% if p.model-status != 'hidden' %}
          <div class="list-group-item">
            <a href="{{ p.url }}">{{ p.title }}</a>
            {% if p.model-status == 'code' %}
              <span class="label label-success pull-right glyph-label">
                  <span class="glyphicon glyphicon-ok" rel="tooltip" title="Code runs"></span>
              </span>
            {% elsif p.model-status == 'static' %}
              <span class="label label-default pull-right glyph-label">
                  <span class="glyphicon glyphicon-align-left" rel="tooltip" title="Static listing ({{ p.model-status-verbose }})"></span>
              </span>
            {% endif %}
          </div>
        {% endif %}
      {% endunless %}
    {% endif %}
  {% endfor %}

</div>
{% endif %}

<div class="btn-toolbar bottom-toolbar pull-right">
    <a class="btn btn-success" id="github-add-link" href="https://github.com/forestdb/forestdb.org/new/gh-pages/models">Add Model</a>
</div>

<script type="text/javascript">
  load_repo_contributors();
</script>
