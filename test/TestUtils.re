let lsDir = dir =>
  Sys.readdir(dir) |> Array.to_list |> List.map(Filename.concat(dir));

let syscall = cmd => {
  let (ic, oc) = Unix.open_process(cmd);
  let buf = Buffer.create(16);
  try(
    while (true) {
      Buffer.add_channel(buf, ic, 1);
    }
  ) {
  | End_of_file => ()
  };
  let _ = Unix.close_process((ic, oc));
  Buffer.contents(buf);
};

let refmtBin = "_esy/default/build/install/default/bin/refmt";
