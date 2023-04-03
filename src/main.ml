
external action_dup2 : unit -> Eio_unix.Private.Fork_action.fork_fn = "eio_unix_fork_dup2"
let action_dup2 = action_dup2 ()
let dup2 oldfd newfd : Eio_unix.Private.Fork_action.t
  = { run = fun k -> k (Obj.repr (action_dup2, oldfd, newfd)) }

let client ~stdout ~stdin ~write ~read =
  Eio.Fiber.both
    (fun () -> Eio.Flow.copy stdin write)
    (fun () -> Eio.Flow.copy read stdout)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let readInpipe, writeInpipe = Eio_unix.pipe sw in
  let readOutpipe, writeOutpipe = Eio_unix.pipe sw in
  let child =
    let read, write = readOutpipe, writeInpipe in
    let
      stdinAction = dup2 (Eio_unix.FD.peek read) Unix.stdin and
      stdoutAction = dup2 Unix.stdout (Eio_unix.FD.peek write) and
      execvAction = Eio_linux.Low_level.Process.Fork_action.execve
        "/run/current-system/sw/bin/bash"
        ~argv:[| "-bash" |]
        ~env:[||]
    in
    Eio_linux.Low_level.Process.spawn ~sw [
      stdinAction;
      stdoutAction;
      execvAction;
    ] in
  client
    ~stdout:(Eio.Stdenv.stdout env)
    ~stdin:(Eio.Stdenv.stdin env)
    ~write:writeOutpipe
    ~read:readInpipe;
  match Eio.Promise.await (Eio_linux.Low_level.Process.exit_status child) with
  | WEXITED s | WSIGNALED s | WSTOPPED s -> exit s
