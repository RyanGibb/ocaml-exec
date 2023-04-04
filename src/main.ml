
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

  (* handle child stopping *)
  let exception Sigchld in
  let sigchld = Eio.Condition.create () in
  let handle_sigchld (_signum : int) = Eio.Condition.broadcast sigchld in
  ignore (Sys.signal Sys.sigchld (Signal_handle handle_sigchld));

  (* handle window size change *)
  match Pty.get_sigwinch () with
  | None -> ()
  | Some sigwinch ->
    let handle_sigwinch (_signum : int) =
      let ws = Pty.tty_window_size () in
      ignore (Pty.set_window_size pty ws);
    in
    handle_sigwinch sigwinch;
    ignore (Sys.signal sigwinch (Signal_handle handle_sigwinch));

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

external action_setup_shell : unit -> Eio_unix.Private.Fork_action.fork_fn = "eio_unix_fork_setup_shell"
let action_setup_shell = action_setup_shell ()
let setup_shell pty : Eio_unix.Private.Fork_action.t
  = { run = fun k -> k (Obj.repr (action_setup_shell, pty)) }

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = Pty.open_pty () in
  (* spawn shell 'server' as child process *)
  let server =
    (* TODO get default shell from /etc/passwd *)
    let
      ptyAction = setup_shell pty and
      execvAction = Eio_linux.Low_level.Process.Fork_action.execve
        "/run/current-system/sw/bin/bash"
        ~argv:[| "-bash" |]
        ~env:[||]
    in Eio_linux.Low_level.Process.spawn ~sw [
      ptyAction;
      execvAction;
    ] in
  client
    ~stdout:(Eio.Stdenv.stdout env)
    ~stdin:(Eio.Stdenv.stdin env)
    pty;
  match Eio.Promise.await (Eio_linux.Low_level.Process.exit_status server) with
  | WEXITED s | WSIGNALED s | WSTOPPED s -> exit s
