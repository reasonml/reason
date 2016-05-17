describe("Syntax highlight: JS", function() {
  beforeEach(require('./setup'));

  beforeEach(function(done) {
    Flatdoc.run({
      fetcher: function(callback) {
        callback(null, '# Hola\n\n```js\nfunction() { x }```');
        done();
      }
    });
  });

  it("Add the correct language class", function() {
    expect($('pre>code').attr('class')).eql('lang-js');
  });

  it("Highlight JS keywords", function() {
    expect($('pre').html()).include('<span class="keyword">function</span>');
  });

  it("Have the correct text", function() {
    expect($('pre').text()).eql('function() { x }');
  });
});
