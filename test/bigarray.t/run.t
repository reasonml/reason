Format bigarray
  $ ../../src/refmt/refmt_impl.exe --print-width 50 ./input.re
  my_big_array3.{
    reallyLongStringThatWillDefinitelyBreakLine
  };
  
  my_big_array3.{
    reallyLongStringThatWillDefinitelyBreakLine,
    reallyLongStringThatWillDefinitelyBreakLine
  };
  
  my_big_array3.{
    reallyLongStringThatWillDefinitelyBreakLine,
    reallyLongStringThatWillDefinitelyBreakLine,
    reallyLongStringThatWillDefinitelyBreakLine
  };
  
  my_big_array3.{
    reallyLongString,
    reallyLongString,
    reallyLongString,
    reallyLongString,
    reallyLongString
  };
  
  my_big_array3.{
    reallyLongStringThatWillDefinitelyBreakLine
  } = 3.0;
  
  my_big_array3.{
    reallyLongStringThatWillDefinitelyBreakLine,
    reallyLongStringThatWillDefinitelyBreakLine
  } = 3.0;
  
  my_big_array3.{
    reallyLongString,
    reallyLongString,
    reallyLongString,
    reallyLongString,
    reallyLongString
  } = 3.0;
