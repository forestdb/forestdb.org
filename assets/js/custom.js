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
