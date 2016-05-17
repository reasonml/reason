describe("Quotes", function() {
  beforeEach(require('./setup'));

  describe("One", function() {
    beforeEach(function(done) {
      Flatdoc.run({
        fetcher: function(callback) {
          callback(null, '"Hello" said the ranger--indeed.');
          done();
        }
      });
    });

    it("Transform curly quotes", function() {
      expect($("body").html()).include("\u201cHello\u201d");
    });

    it("Transform em dashes", function() {
      expect($("body").html()).include("\u2014indeed");
    });
  });

  describe("Two", function() {
    beforeEach(function(done) {
      Flatdoc.run({
        fetcher: function(callback) {
          callback(null, '...That\'s all folks!');
          done();
        }
      });
    });

    it("Transform ellipses", function() {
      expect($("body").html()).include("\u2026");
    });

    it("Transform apostrophes", function() {
      expect($("body").html()).include("That\u2019s");
    });
  });
});
