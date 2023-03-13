
let () =
  Eio_main.run @@ fun env ->
  let pty = Pty.open_pty () in
  (* fork–exec *)
  let pid = Unix.fork () in
  (* if parent *)
  if pid != 0 then (
    (* TODO is there a way to clone tio, rather than calling `tcgetattr` twice? *)
    let savedTio = Unix.tcgetattr Unix.stdin in
    let tio = Unix.tcgetattr Unix.stdin in
    
    (* set raw mode *)

    (* input modes *)
    tio.c_ignpar <- true;
    tio.c_istrip <- false;
    tio.c_inlcr <- false;
    tio.c_igncr <- false;
    tio.c_ixon <- false;
    (* tio.c_ixany <- false; *)
    (* tio.c_iuclc <- false; *)
    tio.c_ixoff <- false;

    (* output modes *)
    tio.c_opost <- false;

    (* control modes *)
    tio.c_isig <- false;
    tio.c_icanon <- false;
    tio.c_echo <- false;
    tio.c_echoe <- false;
    tio.c_echok <- false;
    tio.c_echonl <- false;
    (* tio.c_iexten <- false; *)

    (* special characters *)
    tio.c_vmin <- 1;
    tio.c_vtime <- 0;
    Unix.tcsetattr Unix.stdin TCSADRAIN tio;

    let exception Sigchld in
    let sigchld = Eio.Condition.create () in
    let handle_sigchld (_signum : int) = Eio.Condition.broadcast sigchld in
    ignore (Sys.signal Sys.sigchld (Signal_handle handle_sigchld));
    try
      (* don't close PTY file descriptors *)
      let close_unix = false in
      Eio.Fiber.all [
        (fun () -> Eio.Switch.run @@ fun sw ->
          let sink = Eio_unix.FD.as_socket ~sw ~close_unix pty.Pty.masterfd in
          Eio.Flow.copy (Eio.Stdenv.stdin env) sink);
        (fun () -> Eio.Switch.run @@ fun sw ->
          let source = Eio_unix.FD.as_socket ~sw ~close_unix pty.Pty.masterfd in
          Eio.Flow.copy source (Eio.Stdenv.stdout env)
        );
        (fun () -> Eio.Condition.await_no_mutex sigchld; raise Sigchld);
      ]
      with
      | Sigchld -> ();
    (* restore tio *)
    Unix.tcsetattr Unix.stdin TCSADRAIN savedTio;
  ) else
    Unix.close pty.Pty.masterfd;
    Pty.switch_controlling_pty pty;
    (* Pty.window_size pty pty_window; *)
    Unix.dup2 pty.Pty.slavefd Unix.stdin;
    Unix.dup2 pty.Pty.slavefd Unix.stdout;
    Unix.dup2 pty.Pty.slavefd Unix.stderr;
    Unix.close pty.Pty.slavefd;
    (* TODO get default shell from /etc/passwd *)
    try Unix.execve "/run/current-system/sw/bin/bash"
      (* login shell *)
      [| "-bash"; |]
      (Unix.unsafe_environment ())
      (* [| "PATH=" ^ Unix.getenv "PATH" |];; *)
    with Unix.Unix_error (x,_s,y) ->
      print_endline (Printf.sprintf "%s: %s" y (Unix.error_message x));;
