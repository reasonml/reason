process.stdout.write('Starting test environment');

global.getFile = function(filepath) {
  var path = require('path');
  var fs = require('fs');
  return fs.readFileSync(path.resolve(__dirname, '..', filepath)).toString();
};

global.chai = require('chai');
global.expect = chai.expect;
global.assert = chai.assert;
chai.should();
process.stdout.write('.');

var jsdom = require('jsdom');
process.stdout.write('. OK\n');

module.exports = function(done) {
  jsdom.env({
    html: getFile('test/fixtures/context.html'),
    src: [
      getFile('support/vendor/jquery.js'),
      getFile('flatdoc.js')
    ],
    done: function(errors, window) {
      global.window = window;
      global.$ = window.jQuery;
      global.Flatdoc = window.Flatdoc;
      done(errors);
    }
  });
};
