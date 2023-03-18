Format basic
  $ refmt --print re ./input.re > ./formatted.re

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re
  File "formatted.re", lines 536-548, characters 18-1:
  536 | ..................{
  537 |   f(
  538 |     /* a */
  539 |     1,
  540 |     /* b */
  ...
  545 |     4,
  546 |     /* does work */
  547 |   );
  548 | }.
  Warning 10 [non-unit-statement]: this expression should have type unit.
  File "formatted.re", lines 549-560, characters 18-1:
  549 | ..................{
  550 |   f(
  551 |     /* a */
  552 |     1,
  553 |     /* b */
  ...
  557 |     /* d */
  558 |     4 /* does work */
  559 |   );
  560 | }.
  Warning 10 [non-unit-statement]: this expression should have type unit.

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
