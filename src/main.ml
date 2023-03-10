
let () =
  Eio_main.run @@ fun env ->
  let readInpipe, writeInpipe = Unix.pipe () in
  let readOutpipe, writeOutpipe = Unix.pipe () in
  (* fork–exec *)
  let pid = Unix.fork () in
  (* if parent *)
  if pid != 0 then (
  (* close unused pipes *)
    Unix.close writeInpipe;
    Unix.close readOutpipe;
    let close_unix = true in
    let read, write = readInpipe, writeOutpipe in
      Eio.Fiber.both
        (fun () -> Eio.Switch.run @@ fun sw ->
          let sink = Eio_unix.FD.as_socket ~sw ~close_unix write in
          Eio.Flow.copy (Eio.Stdenv.stdin env) sink)
        (fun () -> Eio.Switch.run @@ fun sw ->
          let source = Eio_unix.FD.as_socket ~sw ~close_unix read in
          Eio.Flow.copy source (Eio.Stdenv.stdout env))
  ) else
    (* close unused pipes *)
    Unix.close writeOutpipe;
    Unix.close readInpipe;
    let read, write = readOutpipe, writeInpipe in
    (* redirect stdin stdout to and from pipe *)
    Unix.dup2 read Unix.stdin;
    Unix.dup2 write Unix.stdout;
    Unix.execve "/run/current-system/sw/bin/bash"
      (* login shell *)
      [| "-bash"; |]
      (Unix.unsafe_environment ());;
      (* [| "PATH=" ^ Unix.getenv "PATH" |];; *)
