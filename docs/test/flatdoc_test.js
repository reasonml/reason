describe("Flatdoc", function() {
  beforeEach(require('./setup'));

  it("Should be present", function() {
    assert.equal('object', typeof window.Flatdoc);
  });

  describe("Simple", function() {
    beforeEach(function(done) {
      Flatdoc.run({
        fetcher: function(callback) {
          callback(null, '# Hola\n\nHey there `world`');
          done();
        }
      });
    });

    it("Should have the right title", function() {
      $('#title').text().should.equal('Hola');
    });

    it("Should have a paragraph", function() {
      $('body').html().should.include('<p>Hey there <code>world</code></p>');
    });

    it("Should have a menu", function() {
      $('#hola-link').length.should.equal(1);
    });

    it("Should have a headline", function() {
      $('h1#hola').length.should.equal(1);
      $('h1#hola').text().should.equal('Hola');
    });
  });
});
