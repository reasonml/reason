describe("jsdom", function() {
  beforeEach(require('./setup'));

  it("environment should work", function() {
    $('body').append('<a id="aaa">hello</a>');
    $('a#aaa').text().should.equal('hello');
  });
});

