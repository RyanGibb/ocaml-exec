
let client ~stdout ~stdin pty =
  let savedTio = Unix.tcgetattr Unix.stdin in

  (* set raw mode *)
  let tio = {
    savedTio with
      (* input modes *)
      c_ignpar = true;
      c_istrip = false;
      c_inlcr = false;
      c_igncr = false;
      c_ixon = false;
      (* c_ixany = false; *)
      (* c_iuclc = false; *)
      c_ixoff = false;

      (* output modes *)
      c_opost = false;

      (* control modes *)
      c_isig = false;
      c_icanon = false;
      c_echo = false;
      c_echoe = false;
      c_echok = false;
      c_echonl = false;
      (* c_iexten = false; *)

      (* special characters *)
      c_vmin = 1;
      c_vtime = 0;
    };
  in Unix.tcsetattr Unix.stdin TCSADRAIN tio;

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
        Eio.Flow.copy stdin sink);
      (fun () -> Eio.Switch.run @@ fun sw ->
        let source = Eio_unix.FD.as_socket ~sw ~close_unix pty.Pty.masterfd in
        Eio.Flow.copy source stdout
      );
      (fun () -> Eio.Condition.await_no_mutex sigchld; raise Sigchld);
    ]
    with
    | Sigchld -> ();
  (* restore tio *)
  Unix.tcsetattr Unix.stdin TCSADRAIN savedTio

let server pty =
  Unix.close pty.Pty.masterfd;
    Pty.switch_controlling_pty pty;
    (* TODO Pty.window_size pty pty_window; *)
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
      print_endline (Printf.sprintf "%s: %s" y (Unix.error_message x));
    Pty.close_pty pty

let () =
  Eio_main.run @@ fun env ->
  let pty = Pty.open_pty () in
  (* fork–exec *)
  let pid = Unix.fork () in
  (* if parent *)
  if pid != 0 then
    client ~stdout:(Eio.Stdenv.stdout env) ~stdin:(Eio.Stdenv.stdin env) pty
  else
    server pty
