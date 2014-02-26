---
layout: default
title: Forest
all-model-statuses: [code, pseudocode, started, stub]
all-model-categories: [Concept Learning, Nested Inference, Inverse Dynamics, Machine Learning, Miscellaneous]
---



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
                    <span class="label label-default pull-right">Stub</span> {{ p.title }}
                  {% else %}
                    {% if p.model-status == 'code' %}
                      <span class="label label-success pull-right">Code</span>            
                    {% elsif p.model-status == 'pseudocode %}
                      <span class="label label-primary pull-right">Pseudocode</span>
                    {% elsif p.model-status == 'started' %}
                      <span class="label label-info pull-right">Started</span>
                    {% else %}
                    {% endif %}
                    <a href="{{ p.url }}">{{ p.title }}</a>          
                  {% endif %}
                  <span class="subtitle">{{ p.model-description }}</span>
              </div>
            {% endif %}
          {% endif %}
        {% endif %}
      {% endfor %}
    {% endfor %}

</div>

  {% endfor %}
  

