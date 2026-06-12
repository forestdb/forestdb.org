---
layout: default
title: Forest - A Repository for Generative Models
isindex: true
all-model-statuses: [code, link, code-fail, stub]
all-model-categories: [Concept Learning, Reasoning about Reasoning, Probabilistic Language Understanding, Counterfactuals and Explanations, Machine Learning, Nonparametric Models, Bayesian Data Analysis, Undirected Constraints, Inverse Dynamics, PPAML Challenge Problems, Miscellaneous]
---

<div class="page-header">
  <h1>Models</h1>
</div>

<p class="text-muted">
Forest is a community repository of generative models written in probabilistic
programming languages, mainly
<a href="https://github.com/probmods/webchurch">Church</a> and
<a href="https://github.com/probmods/webppl">WebPPL</a>.
Most code boxes are editable and run in your browser. Forest is maintained as a
teaching resource and archive; new models are welcome
(<a href="https://github.com/forestdb/forestdb.org#adding-models">how to contribute</a>).
A machine-readable index of all models is available at
<a href="/models.json">models.json</a>.
</p>

{% for category in page.all-model-categories %}

<div class="list-group">

  <div class="list-group-item" style="background-color: #F9F9F9">
    {{ category }}
  </div>

    {% for status in page.all-model-statuses %}
      {% for p in site.pages %}
        {% if p.layout == 'model' %}
          {% if p.model-status == status %}
           {% if p.model-category == category %}
              <div class="list-group-item">
                  {% if p.model-status == 'stub' %}
                    {{ p.title }}
                    <span class="label label-default pull-right glyph-label">
                        <span class="glyphicon glyphicon-asterisk" rel="tooltip" title="Stub"></span>
                    </span>
                  {% else %}
                    <a href="{{ p.url }}">{{ p.title }}</a>
                    {% if p.model-status == 'code' %}
                      <span class="label label-success pull-right glyph-label">
                          <span class="glyphicon glyphicon-ok" rel="tooltip" title="Code runs"></span>
                      </span>
                    {% elsif p.model-status == 'link' %}
                      <span class="label label-success pull-right glyph-label">
                          <span class="glyphicon glyphicon-bookmark" rel="tooltip" title="Link to code"></span>
                      </span>
                    {% elsif p.model-status == 'code-fail' %}
                      <span class="label label-warning pull-right glyph-label">
                          <span class="glyphicon glyphicon-remove" rel="tooltip" title="Code broken ({{ p.model-status-verbose }})"></span>
                      </span>
                    {% else %}
                    {% endif %}
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
            {% if p.model-status == 'stub' %}
              {{ p.title }}
              <span class="label label-default pull-right glyph-label">
                  <span class="glyphicon glyphicon-asterisk" rel="tooltip" title="Stub"></span>
              </span>
            {% else %}
              <a href="{{ p.url }}">{{ p.title }}</a>
              {% if p.model-status == 'code' %}
                <span class="label label-success pull-right glyph-label">
                    <span class="glyphicon glyphicon-ok" rel="tooltip" title="Code runs"></span>
                </span>
              {% elsif p.model-status == 'code-fail' %}
                <span class="label label-warning pull-right glyph-label">
                    <span class="glyphicon glyphicon-remove" rel="tooltip" title="Code broken ({{ p.model-status-verbose }})"></span>
                </span>
              {% endif %}
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
