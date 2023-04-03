
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

external action_dup2 : unit -> Eio_unix.Private.Fork_action.fork_fn = "eio_unix_fork_dup2"
let action_dup2 = action_dup2 ()
let dup2 oldfd newfd : Eio_unix.Private.Fork_action.t
  = { run = fun k -> k (Obj.repr (action_dup2, oldfd, newfd)) }

external action_switch_controlling_pty : unit -> Eio_unix.Private.Fork_action.fork_fn = "eio_unix_fork_switch_controlling_pty"
let action_switch_controlling_pty = action_switch_controlling_pty ()
let switch_controlling_pty pty : Eio_unix.Private.Fork_action.t
  = { run = fun k -> k (Obj.repr (action_switch_controlling_pty, pty)) }


let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = Pty.open_pty () in
  (* spawn shell 'server' as child process *)
  let server =
    (* TODO Pty.window_size pty pty_window; *)
    (* TODO get default shell from /etc/passwd *)
    let
      ptyAction = switch_controlling_pty pty and
      stdinAction = dup2 pty.Pty.slavefd Unix.stdin and
      stdoutAction = dup2 pty.Pty.slavefd Unix.stdout and
      stderrAction = dup2 pty.Pty.slavefd Unix.stderr and
      execvAction = Eio_linux.Low_level.Process.Fork_action.execve
        "/run/current-system/sw/bin/bash"
        ~argv:[| "-bash" |]
        ~env:[||]
    in Eio_linux.Low_level.Process.spawn ~sw [
      ptyAction;
      stdinAction;
      stdoutAction;
      stderrAction;
      execvAction;
    ] in
  client
    ~stdout:(Eio.Stdenv.stdout env)
    ~stdin:(Eio.Stdenv.stdin env)
    pty;
  match Eio.Promise.await (Eio_linux.Low_level.Process.exit_status server) with
  | WEXITED s | WSIGNALED s | WSTOPPED s -> exit s
