Format issue #1696 - refmt does not preserve original placing of comments
  $ refmt ./input.re | tee formatted.re
  [@bs.send.pipe: T.t]
  external on:
    (
      [@bs.string] [
        /* EventEmitter events */
        | [@bs.as "newListener"]
          `NewListener((string, 'any) => unit)
        | [@bs.as "removeListener"]
          `RemoveListener((string, 'any) => unit)
        /* Stream events */
        /* Readable Stream events */
        | [@bs.as "close"] `Close(unit => unit)
        | [@bs.as "error"] `Error(Error.t => unit)
        | [@bs.as "data"] `Data(Node.string_buffer => unit)
        | [@bs.as "end"] `End(unit => unit)
        | [@bs.as "readable"] `Readable(unit => unit)
      ]
    ) =>
    T.t;

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

