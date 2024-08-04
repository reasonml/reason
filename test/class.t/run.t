See the typed tree from ./input.re
  $ cat ./input.re | ../lib/outcometreePrinter.exe
  class aClass1 :
    ('a) =>
    {
      pub a1: int;
      pub a2: unit => int;
      pub a3: (int, int) => int;
      pub a4: (int, int) => int
    };
  class aClass2 : ('a) => {  };
  class aClass3 : (int, int) => {  };
  class aClass4 : (int, int, int) => {  };
  class aClass5 : (int, int, int) => {  };
  class aClass6 : (int => int, int) => {  };
  class aClass7 : (int, int, int) => {  };
  class labeledClass1 : ('a) => {  };
  class labeledClass2 : (~x: int, int) => {  };
  class labeledClass3 : (~x: int, int, int) => {  };
  class labeledClass4 : (~x: int, int, int) => {  };
  class labeledClass5 : ((~y: int) => int, int) => {  };
  class labeledClass6 : (~x: (int, int), int) => {  };
