Format issue #2025 - switch and complex tuple
  $ refmt ./input.re | tee formatted.re
  switch (foo) {
  | None => (
      {
        delta: {
          x: 0,
          y: 0,
        },
      },
      Tag,
    )
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

