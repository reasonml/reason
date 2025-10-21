Format issue #1437 - outputs [@explicit_arity] even though it takes a tuple
  $ refmt ./input.re | tee formatted.re
  type msg =
    | Request(command_msg, option(updateOp));
  
  let msgToString =
    fun
    | Request(cmd, _) =>
      "Request\n  cmd: " ++ Command.toString(cmd);

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

