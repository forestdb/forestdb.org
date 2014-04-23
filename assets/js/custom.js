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


// Fuzzy autocomplete search + fast navigation

function substringMatcher(strs) {
    return function findMatches(q, cb) {
        var matches, substringRegex;
        matches = [];
        q = q.split("").reduce(function (a, b) {
            return a + '[^' + b + ']*' + b;
        });
        var substrRegex = new RegExp(q, 'i');
        $.each(strs, function (i, str) {
            if (substrRegex.test(str)) {
                if (matches.length < 10) {
                    matches.push({
                        value: str
                    });
                };
            };
        });
        cb(matches);
    };
};

$(function () {
    $('#cse-text').typeahead({
        hint: true,
        highlight: true,
        minLength: 1
    }, {
        name: 'model_names',
        displayKey: 'value',
        source: substringMatcher(model_names)
    });
    $('#cse-text').bind('typeahead:selected', function (obj, datum, name) {
        console.log(datum.value);
        for (var i = 0; i < model_names.length; i++) {
            if (model_names[i] == datum.value) {
                document.location.href = model_urls[i];
                return true;
            };
        };
        return true;
    });
})


// Code boxes

$(function () {
    var code_pres = $("pre").filter(function () {
        return ($(this).find('code').length === 1) && (!$(this.parentNode).is("blockquote"));
    });
    $.each(code_pres,
        function (index, pre) {
            var defaultText = $(pre).find("code").text();
            require('./editor').injector(pre, {
                defaultText: defaultText,
                defaultEngine: "webchurch"
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

function format_reference(citation) {
    var s = "<em>" + citation["AUTHOR"] + " (" + citation["YEAR"] + ")</em>";
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


// Analytics

(function (i, s, o, g, r, a, m) {
    i['GoogleAnalyticsObject'] = r;
    i[r] = i[r] || function () {
        (i[r].q = i[r].q || []).push(arguments);
    }, i[r].l = 1 * new Date();
    a = s.createElement(o),
    m = s.getElementsByTagName(o)[0];
    a.async = 1;
    a.src = g;
    m.parentNode.insertBefore(a, m);
})(window, document, 'script', '//www.google-analytics.com/analytics.js', 'ga');

ga('create', 'UA-54996-10', 'forestdb.org');
ga('require', 'linkid', 'linkid.js');
ga('send', 'pageview');


// Contributors

function load_contributors(url) {
    $.getJSON(url, function(data) {
        var consumed_authors = {};
        $.each(data, function(index, item) {
            if (item.author) {
                // item is a commit object
                var author = item.author;
            } else {
                // item is a user object
                var author = item;
            };
            var id = author.login || author.email;
            if (consumed_authors[id]) {
                return;
            };
            consumed_authors[id] = true;
            var author_ref_html = $("<span />");
            author_ref_html.append($("<img />", {
                "src" : author.avatar_url + "s=16",
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
