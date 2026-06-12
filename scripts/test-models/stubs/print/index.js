// `print` is provided by the webppl editor in the browser; provide it headless.
// Bare webppl function calls follow the CPS protocol (store, continuation, address, ...args).
module.exports = function(s, k, a, x) {
  console.log(x);
  return k(s);
};
