let () =
  Eio_main.run @@ fun env ->
  let pty = Pty.open_pty () in
  (* fork–exec *)
  let pid = Unix.fork () in
  (* if parent *)
  if pid != 0 then (
    let close_unix = true in
    Eio.Fiber.both
      (fun () -> Eio.Switch.run @@ fun sw ->
        let sink = Eio_unix.FD.as_socket ~sw ~close_unix pty.Pty.masterfd in
        Eio.Flow.copy (Eio.Stdenv.stdin env) sink)
      (fun () -> Eio.Switch.run @@ fun sw ->
        let source = Eio_unix.FD.as_socket ~sw ~close_unix pty.Pty.masterfd in
        Eio.Flow.copy source (Eio.Stdenv.stdout env))
  ) else
    Unix.close pty.Pty.masterfd;
    Pty.switch_controlling_pty pty;
    (* Pty.window_size pty pty_window; *)
    Unix.dup2 pty.Pty.slavefd Unix.stdin;
    Unix.dup2 pty.Pty.slavefd Unix.stdout;
    Unix.dup2 pty.Pty.slavefd Unix.stderr;
    Unix.close pty.Pty.slavefd;
    try Unix.execve "/run/current-system/sw/bin/bash"
      (* login shell *)
      [| "-bash"; |]
      (Unix.unsafe_environment ())
      (* [| "PATH=" ^ Unix.getenv "PATH" |];; *)
    with Unix.Unix_error (x,_s,y) ->
      print_endline (Printf.sprintf "%s: %s" y (Unix.error_message x));;
