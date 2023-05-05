Format basic
  $ refmt --print re ./input.re > ./formatted.re

Print the formatted file
  $ cat ./formatted.re
  /* https://github.com/facebook/reason/issues/2038 */
  let my_big_array1 =
    Bigarray.Array1.create(
      Bigarray.float32,
      Bigarray.c_layout,
      20,
    );
  
  my_big_array1.{1};
  
  my_big_array1.{1} = 1.0;
  
  let my_big_array2 =
    Bigarray.Array2.create(
      Bigarray.float32,
      Bigarray.c_layout,
      20,
      20,
    );
  
  my_big_array2.{1, 2};
  
  my_big_array2.{1, 2} = 1.0;
  
  let my_big_array3 =
    Bigarray.Array3.create(
      Bigarray.float32,
      Bigarray.c_layout,
      20,
      20,
      20,
    );
  
  my_big_array3.{1, 2, 3};
  
  my_big_array3.{1, 2, 3} = 1.0;
  
  let reallyLongStringThatWillDefinitelyBreakLine = 0;
  
  my_big_array3.{
    reallyLongStringThatWillDefinitelyBreakLine,
    reallyLongStringThatWillDefinitelyBreakLine,
    reallyLongStringThatWillDefinitelyBreakLine
  };
  
  my_big_array3.{
    reallyLongStringThatWillDefinitelyBreakLine,
    reallyLongStringThatWillDefinitelyBreakLine,
    reallyLongStringThatWillDefinitelyBreakLine
  } = 3.0;
Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
