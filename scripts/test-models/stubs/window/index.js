// Headless stand-in for the browser window global as the dippl-era bundle
// exposed it: ERP constructors live on window in the browser.
module.exports = require('../../node_modules/webppl-0-6-1/src/erp.js');
