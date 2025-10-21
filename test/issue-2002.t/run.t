Format issue #2002 - Function parameters don't always wrap with labeled and optional parameters
  $ refmt ./input.re | tee formatted.re
  let function_with_a_long_name =
      (~many=?, parameters, contained_in, this, definition, arguably, too_many) => {
    ();
    ();
  };
  
  let function_with_a_long_name =
      (
        many,
        parameters,
        contained_in,
        this,
        definition,
        arguably,
        too_many,
        wow,
      ) => {
    ();
    ();
  };
  
  let function_with_a_long_name =
      (
        ~many=?,
        ~parameters,
        ~contained_in,
        this,
        definition,
        arguably,
        too_many,
      ) => {
    ();
    ();
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

