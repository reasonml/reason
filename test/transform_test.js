describe("Flatdoc.transformer", function() {
  var trans;

  beforeEach(require('./setup'));
  beforeEach(function() {
    trans = Flatdoc.transformer;
  });

  it(".addIDs()", function() {
    var $html = $("<div><h1>Hello</h1><h2>One Two &amp; Three</h2>");
    trans.addIDs($html);

    $html.find('h1').attr('id').should.equal('hello');
    $html.find('h2').attr('id').should.equal('hello-one-two-three');
  });

  it(".buttonize()", function() {
    var $html = $("<div><a class='small' href='#'>Hello &gt;</a>");
    trans.buttonize($html);

    assert($html.find('a').is('.button'));
    assert($html.find('a').is('.small'));
  });
});

