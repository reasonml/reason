Format issue #2398 - Change indentation of functions with many arguments
  $ refmt ./input.re | tee formatted.re
  let makeCallback =
      (
        ~target,
        ~method,
        ~getHeader,
        ~create_response,
        ~respond_with_string,
        ~headers_of_list,
        ~read_body,
        ~headers,
        ~context: Context.t,
        reqd,
      ) => {
    open Lwt.Infix;
    ();
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

