Format issue #1795 - Location of () and simple expressions not retained correctly
  $ refmt ./input.re | tee formatted.re
  if (Sys.file_exists(raw_path)) {
    /* The directory exists, stop recursing but check error cases */
    if (Sys.is_directory(raw_path)) {
      "asdf";
      /* All good */
    } else {
      ();
    };
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

