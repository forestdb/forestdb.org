---
layout: default
title: Forest - A Repository for Generative Models
isindex: true
all-model-statuses: [code, link, code-fail, stub]
all-model-categories: [Concept Learning, Reasoning about Reasoning, Machine Learning, Nonparametric Models, Undirected Constraints, Inverse Dynamics, Miscellaneous]
---

<div class="page-header">
  <h1>Models<br /></h1>
</div>

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

</div>

{% endfor %}

<div class="btn-toolbar bottom-toolbar pull-right">
    <a class="btn btn-success" id="github-add-link" href="https://github.com/forestdb/forestdb.org/new/gh-pages/models">Add Model</a>
</div>

<script type="text/javascript">
  load_repo_contributors();
</script>
