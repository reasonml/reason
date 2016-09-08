describe("Syntax highlight: HTML", function() {
  beforeEach(require('./setup'));

  beforeEach(function(done) {
    Flatdoc.run({
      fetcher: function(callback) {
        callback(null, '# Hola\n\n```html\n<span class="x">hi</span>```');
        done();
      }
    });
  });

  it("Add the correct language class", function() {
    expect($('pre>code').attr('class')).eql('lang-html');
  });

  it("Highlight strings", function() {
    expect($('pre').html()).include('<span class="string">"x"</span>');
  });

  it("Highlight tags", function() {
    expect($('pre').html()).include('<span class="keyword">span</span>');
  });

  it("Escape &lt;", function() {
    expect($('pre').html()).include('&lt;');
  });

  it("Escape &gt;", function() {
    expect($('pre').html()).include('&gt;');
  });

  it("Have the correct text", function() {
    expect($('pre').text()).eql('<span class="x">hi</span>');
  });
});
