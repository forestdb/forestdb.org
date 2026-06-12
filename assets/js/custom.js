"use strict";


// Github repository links and buttons

var github_repository = "https://github.com/forestdb/forestdb.org/";
var repository_api_url = "https://api.github.com/repos/forestdb/forestdb.org/";

function markdown_url(page_url) {
    return page_url.slice(0, -4) + "md";
}

function github_edit_url(page_url) {
    return github_repository + "edit/gh-pages" + markdown_url(page_url);
}

function github_delete_url(page_url) {
    return github_repository + "delete/gh-pages" + markdown_url(page_url);
}

function github_page_url(page_url) {
    if (page_url == "/index.html") {
        return github_repository;
    } else {
        return github_repository + "blob/gh-pages" + markdown_url(page_url);
    };
}


// Search: fuzzy typeahead over titles, backed by a lunr.js full-text
// index over model prose (built client-side from /search.json, which is
// generated at build time). Title matches always rank above prose matches.
// If JS or the index fails, the server-rendered model list still works.

var search_docs = null;     // [{title, url, category, tags, content}, ...]
var search_index = null;    // lunr index over search_docs
var search_loading = false;

function load_search_index() {
    if (search_docs || search_loading) { return; };
    if (typeof lunr === 'undefined') { return; };
    search_loading = true;
    $.getJSON('/search.json', function (data) {
        var docs = data.models || [];
        search_index = lunr(function () {
            this.ref('i');
            this.field('title', { boost: 10 });
            this.field('tags', { boost: 5 });
            this.field('content');
            for (var i = 0; i < docs.length; i++) {
                this.add({
                    i: i,
                    title: docs[i].title || '',
                    tags: docs[i].tags || '',
                    content: docs[i].content || ''
                });
            };
        });
        search_docs = docs;
    }).fail(function () {
        search_loading = false;
    });
}

// Docs to fuzzy-match titles against: search.json once loaded, otherwise
// the titles/urls inlined into every page (no categories, but available
// immediately and without the full-text index).
function title_docs() {
    if (search_docs) { return search_docs; };
    var docs = [];
    for (var i = 0; i < model_names.length; i++) {
        docs.push({ title: model_names[i], url: model_urls[i], category: null });
    };
    return docs;
}

function fuzzy_regex(q) {
    q = q.split("").map(function (c) {
        return c.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
    }).reduce(function (a, b) {
        return a + '[^' + b + ']*' + b;
    });
    try {
        return new RegExp(q, 'i');
    } catch (e) {
        return null;
    };
}

function lunr_search(query) {
    if (!search_index) { return []; };
    try {
        return search_index.query(function (q) {
            lunr.tokenizer(query).forEach(function (token) {
                var term = token.toString();
                q.term(term, {});
                q.term(term, { wildcard: lunr.Query.wildcard.TRAILING });
            });
        });
    } catch (e) {
        return [];
    };
}

function model_searcher(query, cb) {
    var max_results = 10;
    var results = [];
    var seen = {};
    function push(doc, kind) {
        if (seen[doc.url]) { return; };
        seen[doc.url] = true;
        results.push({
            value: doc.title,
            url: doc.url,
            category: doc.category,
            kind: kind
        });
    }
    // 1. Title matches first (original typeahead behavior): exact
    //    substring matches rank above scattered fuzzy matches.
    var docs = title_docs();
    var regex = fuzzy_regex(query);
    var lower = query.toLowerCase();
    var substring_matches = [];
    var fuzzy_matches = [];
    for (var i = 0; i < docs.length; i++) {
        var title = docs[i].title || '';
        if (title.toLowerCase().indexOf(lower) !== -1) {
            substring_matches.push(docs[i]);
        } else if (regex && regex.test(title)) {
            fuzzy_matches.push(docs[i]);
        };
    };
    var title_matches = substring_matches.concat(fuzzy_matches);
    for (var t = 0; t < title_matches.length && results.length < max_results; t++) {
        push(title_matches[t], 'title');
    };
    // 2. Then full-text matches over model prose (and code), best first.
    if (search_docs) {
        var hits = lunr_search(query);
        for (var j = 0; j < hits.length && results.length < max_results; j++) {
            push(search_docs[parseInt(hits[j].ref, 10)], 'prose');
        };
    };
    cb(results);
}

function escape_html(s) {
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;')
        .replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

function suggestion_html(datum) {
    var html = '<p>' + escape_html(datum.value);
    if (datum.category) {
        html += ' <span class="tt-category text-muted">&mdash; ' +
            escape_html(datum.category) + '</span>';
    };
    html += '</p>';
    return html;
}

$(function () {
    var input = $('#cse-text');
    if (input.length === 0) { return; };
    // Fetch the full-text index lazily, on first interaction with the box.
    input.one('focus keydown', load_search_index);
    input.typeahead({
        hint: true,
        highlight: true,
        minLength: 1
    }, {
        name: 'models',
        displayKey: 'value',
        source: model_searcher,
        templates: {
            suggestion: suggestion_html,
            empty: '<div class="tt-empty text-muted">No matching models</div>'
        }
    });
    input.bind('typeahead:selected', function (obj, datum, name) {
        document.location.href = datum.url;
        return true;
    });
    // On Enter, go to the first match for the current query.
    $('#model-search-box').submit(function (e) {
        e.preventDefault();
        var query = input.val();
        if (!query) { return; };
        model_searcher(query, function (matches) {
            if (matches.length > 0) {
                document.location.href = matches[0].url;
            };
        });
    });
})


// References and bibliography

var textohtml_map = {
    "\\\"u": "&uuml;",
    "\\\"a": "&auml;",
    "\\\"o": "&ouml;",
    "\\'e": "&eacute;",
    "\\\"U": "&Uuml;",
    "\\\"A": "&Auml;",
    "\\\"O": "&Ouml;",
    "\\'E": "&Eacute;"
};

function textohtml(tex) {
    for (var key in textohtml_map) {
        if (textohtml_map.hasOwnProperty(key)) {
            tex = tex.replace("{" + key + "}", textohtml_map[key]);
            tex = tex.replace(key, textohtml_map[key]);
        };
    };
    return tex;
}

function replace_html(source, target) {
    $('p, li').each(function () {
        var html = $(this).html();
        $(this).html(html.replace(new RegExp(source, "ig"), target));
    });
}

function format_citation(citation) {
  var s = "";
  if (citation["URL"]) {
    s += "<a href='" + citation["URL"] + "'>" + citation["TITLE"] + "</a>. ";
  } else {
    s += citation["TITLE"] + ". ";
  };
  s += citation["AUTHOR"] + " (" + citation["YEAR"] + ").";
  if (citation["JOURNAL"]) {
    s += " <em>" + citation["JOURNAL"] + "</em>.";
  }
  return textohtml(s);
}

function author_lastname(authorString) {
  var names = authorString.split(", ");
  if (names.length == 0) {
    console.error('Expected first and last name, got: ' + authorString);
    return;
  }
  return names[0];
}

function short_authors(authorsString) {
  if (!authorsString) {
    console.warn('short_authors got:' + authorsString);
    return;
  }
  var authors = authorsString.split(" and ");
  if (authors.length === 0) {
    console.error('Expected >= 1 author, got: ' + authorsString);
    return authorsString;
  }
  var firstAuthor = authors[0];
  if (authors.length === 1) {
    return author_lastname(firstAuthor);
  } else if (authors.length === 2) {
    var secondAuthor = authors[1];
    return author_lastname(firstAuthor) + ' and ' + author_lastname(secondAuthor);
  } else {
    return author_lastname(firstAuthor) + ' et al.';
  }
}

function cite_url(citation) {
  if (citation["URL"]) {
    return citation["URL"];
  }
  return 'https://scholar.google.com/scholar?q="' + citation["TITLE"] + '"';
}

function format_reference(citation) {
  var s = "";
  s += "<a class='ref' href='" + cite_url(citation) + "'>";
  s += short_authors(citation["AUTHOR"]) + " (" + citation["YEAR"] + ")";
  s += "</a>";
  return textohtml(s);  
}

$.get("/bibliography.bib", function (bibtext) {
    $(function () {
        var bibs = doParse(bibtext);
        $.each(
            bibs,
            function (citation_id, citation) {
                replace_html("cite:" + citation_id, format_citation(citation));
                replace_html("ref:" + citation_id, format_reference(citation));
            }
        );
    });
});


// Contributors

function load_contributors(url) {
    $.getJSON(url, function(data) {
        var consumed_authors = {};
        $.each(data, function(index, item) {
            // The commits endpoint returns commit objects (GitHub user in
            // `.author`, which is null for email-only commits); the
            // contributors endpoint returns user objects directly.
            var author = item.author || (item.login ? item : null);
            // Skip commits with no linked GitHub account: they have no
            // avatar or profile, and rendered as a broken thumbnail before.
            if (!author || !author.avatar_url) {
                return;
            };
            var id = author.login || author.email;
            if (consumed_authors[id]) {
                return;
            };
            consumed_authors[id] = true;
            var sep = author.avatar_url.indexOf("?") === -1 ? "?" : "&";
            var author_ref_html = $("<span />");
            author_ref_html.append($("<img />", {
                "src" : author.avatar_url + sep + "s=16",
                "class" : "avatar",
                "width" : "16px",
                "height" : "16px",
                "rel" : "tooltip",
                "title" : id }));
            var author_html = $(
                "<a />",
                { "href" : author.html_url,
                  "html" : author_ref_html
                });
            $("#contributors").append(author_html);
            $(".avatar").tooltip({'placement': 'top'});
        });
    });
}

function load_page_contributors(page_url) {
    var filename = markdown_url(page_url);
    var url = repository_api_url + "commits?path=" + filename;
    load_contributors(url);
}

function load_repo_contributors() {
    var url = repository_api_url + "contributors";
    load_contributors(url);
}


// Tooltips

function initialize_tooltips(){
    $("[rel=tooltip]").tooltip({
        'selector': '',
        'container': 'body',
        'placement': 'right' });
}

$(initialize_tooltips);
