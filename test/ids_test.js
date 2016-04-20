describe("IDs", function() {
  beforeEach(require('./setup'));

  beforeEach(function(done) {
    Flatdoc.run({
      fetcher: function(callback) {
        callback(null, '# a\n\n## b\n\n### c');
        done();
      }
    });
  });

  describe("Menu items", function() {
    it("Level 1", function() {
      expect($("#menu li#a-item")).have.length(1);
      expect($("#menu li#a-item").attr('class')).include('level-1');
    });

    it("Level 2", function() {
      expect($("#menu li#a-b-item")).have.length(1);
      expect($("#menu li#a-b-item").attr('class')).include('level-2');
    });

    it("Level 3", function() {
      expect($("#menu li#a-b-c-item")).have.length(1);
      expect($("#menu li#a-b-c-item").attr('class')).include('level-3');
    });

    it("Level 2 list", function() {
      expect($("#menu ul#a-list")).have.length(1);
      expect($("#menu ul#a-list").attr('class')).include('level-2');
    });

    it("Level 3 list", function() {
      expect($("#menu ul#a-b-list")).have.length(1);
      expect($("#menu ul#a-b-list").attr('class')).include('level-3');
    });
  });

  describe("Headings", function() {
    it("Level 1", function() {
      expect($("#content h1#a")).have.length(1);
    });

    it("Level 2", function() {
      expect($("#content h2#a-b")).have.length(1);
    });

    it("Level 3", function() {
      expect($("#content h3#a-b-c")).have.length(1);
    });
  });
});
