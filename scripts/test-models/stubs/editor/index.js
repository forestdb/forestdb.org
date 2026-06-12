// Stand-in for the wpEditor `editor` global (browser-only).
//
// In the browser, code boxes on a page share one JS heap, so a box can do
// `editor.put('f', f)` and a later box can `editor.get('f')` to chain state
// (including functions) across boxes. The headless runner mirrors this by
// concatenating a box with the earlier boxes that define what it gets, so a
// simple in-process store is enough.
var store = {};
module.exports = {
  put: function(key, value) { store[key] = value; },
  get: function(key) { return store[key]; },
  quiet: function() {},
  MCMCProgress: function() { return function() {}; }
};
