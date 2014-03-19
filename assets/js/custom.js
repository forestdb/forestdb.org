// Github repository links and buttons

github_repository = "https://github.com/forestdb/forestdb.org/"

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

var substringMatcher = function(strs) {
  return function findMatches(q, cb) {
    var matches, substringRegex;
    matches = [];
    q = q.split("").reduce(function(a,b){ return a+'[^'+b+']*'+b; }); 
    substrRegex = new RegExp(q, 'i');
    $.each(strs, function(i, str) {
      if (substrRegex.test(str)) {
        if (matches.length < 10) {
            matches.push({ value: str });
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
  },
  {
    name: 'model_names',
    displayKey: 'value',
    source: substringMatcher(model_names)
  });
  $('#cse-text').bind('typeahead:selected', function(obj, datum, name) {
      console.log(datum.value);
      for (var i = 0; i < model_names.length; i++) {
          if (model_names[i] == datum.value) {
              document.location.href = model_urls[i];
              return true;
          };
      };
      console.log("not found");
      return true;
  });
})
