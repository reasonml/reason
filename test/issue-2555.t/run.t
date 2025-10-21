Format issue #2555 - Broken indentation within extension point when used with include
  $ refmt ./input.re | tee formatted.re
  include [%form
            type input = {name: string};
            let validators = {
              name: {
                strategy: OnSubmit,
                validate: ({name}) => Ok(name),
              },
            }
          ];

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

