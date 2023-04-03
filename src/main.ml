
let client ~stdout ~stdin ~write ~read =
  Eio.Fiber.both
    (fun () -> Eio.Flow.copy stdin write)
    (fun () -> Eio.Flow.copy read stdout)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Could use Eio_unix.pipe with higher level Eio process lib
     but need Eio_linux.FD for Eio_linux.Low_level.Process.spawn *)
  let child =
    let action = Eio_posix.Low_level.Process.Fork_action.execve
      "/run/current-system/sw/bin/bash"
      ~argv:[| "-bash" |]
      ~env:[||] in
    Eio_linux.Low_level.Process.spawn ~sw [ action ] in
  let readInpipe, _writeInpipe = Eio_unix.pipe sw in
  let _readOutpipe, writeOutpipe = Eio_unix.pipe sw in
  client
    ~stdout:(Eio.Stdenv.stdout env)
    ~stdin:(Eio.Stdenv.stdin env)
    ~write:writeOutpipe
    ~read:readInpipe;
  ignore @@ Eio.Promise.await (Eio_linux.Low_level.Process.exit_status child);;
