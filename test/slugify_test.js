describe("Slugify", function () {
  beforeEach(require('./setup'));

  assert("hello", "hello");
  assert("Hey", "hey");
  assert("With Spaces", "with-spaces");
  assert("with numbers (200)", "with-numbers-200");
  assert("french raison d'être", "french-raison-detre");
  assert("greek γιαούρτι", "greek-giaoyrti"); // actually giaourti?

  function assert(input, output) {
    it(input, function() {
      expect(Flatdoc.slugify(input)).eql(output);
    });
  }
});
