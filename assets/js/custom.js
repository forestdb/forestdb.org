"use strict";


// Github repository links and buttons

var github_repository = "https://github.com/forestdb/forestdb.org/";

function github_edit_url(page_url) {
    return github_repository + "edit/gh-pages" + page_url.slice(0, -4) + "md";
}

function github_delete_url(page_url) {
    return github_repository + "delete/gh-pages" + page_url.slice(0, -4) + "md";
}

function github_page_url(page_url) {
    if (page_url == "/index.html") {
        return github_repository;
    } else {
        return github_repository + "blob/gh-pages" + page_url.slice(0, -4) + "md";
    };
}


// Fuzzy autocomplete search + fast navigation

var substringMatcher = function (strs) {
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

    $(".code-controls button").addClass("btn btn-default");
    $(".code-controls").addClass("btn-toolbar pull-right");
    $(".code-controls").after("<div class=\"code-controls-clear\"></div>");
    $(".code-controls button").css("padding", "none");
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

var textohtml = function (tex) {
    for (var key in textohtml_map) {
        if (textohtml_map.hasOwnProperty(key)) {
            tex = tex.replace("{" + key + "}", textohtml_map[key]);
            tex = tex.replace(key, textohtml_map[key]);
        };
    };
    return tex;
}

var replace_html = function (source, target) {
    $('p, li').each(function () {
        var html = $(this).html();
        $(this).html(html.replace(new RegExp(source, "ig"), target));
    });
}

var format_citation = function (citation) {
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

var format_reference = function (citation) {
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
ga('send', 'pageview');