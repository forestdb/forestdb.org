// No-op stand-in for webppl-viz, which only works in the browser.
// Called as a bare function, webppl passes (store, continuation, address, ...args)
// and expects the continuation to be invoked.
var noop = function() {};
var viz = function(s, k, a) { return k(s); };
['auto', 'hist', 'bar', 'line', 'scatter', 'density', 'table', 'marginals',
 'heatMap', 'parCoords', 'casd', 'print', 'vegaPrint'].forEach(function(name) {
  viz[name] = noop;
});
module.exports = viz;
