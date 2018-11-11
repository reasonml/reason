foo #= bar[0];

foo["bar"][0] = 3;

foo["bar"][0]["baz"][1] = 3;

foo["bar"][0]["baz"][1];

foo["bar"] = bar[0];

foo["bar"]["baz"] = bar["baz"][0];

foo[bar + 1];

foo.[bar + 1];

foo.{bar + 1};

foo.[bar + 1] = 1;

foo.{bar + 1} = 1;

foo[bar + 1] = 1;

bla #= Constr(x);

bla #= M.(someFunc(Some(10)));

bla["foo"] = 3;

bla["foo"][0] = 3;

bla["foo"].qux["x"] = 3;

bla["foo"].qux[0]["bar"] = 3;

bla[0]["foo"].qux[0]["bar"] = 3;

bla[0]["foo"].qux[0].[0]["bar"] = 3;

bla[0]["foo"].qux[0].{0}["bar"] = 3;

bla[0]["foo"].qux[0].{0, 1, 2}["bar"] = 3;
