type msg =
  | Request (command_msg, option updateOp);

let msgToString = fun
  | Request (cmd, _) => "Request\n  cmd: " ^ Command.toString cmd;

