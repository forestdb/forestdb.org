function github_edit_url(page_url) {
   return "https://github.com/stuhlmueller/forestdb.org/edit/gh-pages" + page_url.slice(0, -4) + "md";
}

function github_delete_url(page_url) {
   return "https://github.com/stuhlmueller/forestdb.org/delete/gh-pages" + page_url.slice(0, -4) + "md";
}

function github_page_url(page_url) {
   if (page_url == "/index.html") {
       return "https://github.com/stuhlmueller/forestdb.org/";
   } else {
       return "https://github.com/stuhlmueller/forestdb.org/blob/gh-pages" + page_url.slice(0, -4) + "md";
   };
}
