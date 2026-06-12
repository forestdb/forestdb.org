// No-op stand-in for the wpEditor `editor` global (browser-only).
module.exports = {
  put: function() {},
  get: function() { return {}; },
  quiet: function() {},
  MCMCProgress: function() { return function() {}; }
};
