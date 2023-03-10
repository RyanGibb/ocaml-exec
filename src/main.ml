
let () =
  let readInpipe, writeInpipe = Unix.pipe () in
  let readOutpipe, writeOutpipe = Unix.pipe () in
  (* fork–exec *)
  let pid = Unix.fork () in
  (* If parent *)
  if pid != 0 then (
  (* close unused pipes *)
    Unix.close writeInpipe;
    Unix.close readOutpipe;
    let read, write = readInpipe, writeOutpipe in
    (* Unix.dup2 Unix.stdin read;
    Unix.dup2 Unix.stdout write; *)
    let _ = Unix.write_substring write "echo hi\n" 0 8 in
    let buf = Bytes.create 100 in
    let len = Unix.read read buf 0 100 in
    let s = Bytes.sub_string buf 0 len in
    print_endline s;
    let _pid, _status = Unix.wait () in
    ();
  ) else
    (* close unused pipes *)
    Unix.close writeOutpipe;
    Unix.close readInpipe;
    let read, write = readOutpipe, writeInpipe in
    Unix.dup2 read Unix.stdin;
    Unix.dup2 write Unix.stdout;
    (* redirect stdin stdout to and from pipe *)
    (* close unused pipes *)
    (* redirect stdin to and from pipe *)
    Unix.execve "/run/current-system/sw/bin/bash" [| |] [| |]
