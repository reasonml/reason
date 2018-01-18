class type _module ('provider_impl) = {

};
type t;
class type bzz = {
  inherit _module(t)
};

class type t = { as 'a;
  constraint 'a = #s
};
