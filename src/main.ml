
let client ~stdout ~stdin ~write ~read =
  Eio.Fiber.both
    (fun () -> Eio.Switch.run @@ fun sw ->
      let sink = Eio_unix.FD.as_socket ~sw ~close_unix:false (Eio_linux.FD.to_unix `Take write) in
      Eio.Flow.copy stdin sink)
    (fun () -> Eio.Switch.run @@ fun sw ->
      let source = Eio_unix.FD.as_socket ~sw ~close_unix:false (Eio_linux.FD.to_unix `Take read) in
      Eio.Flow.copy source stdout)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Could use Eio_unix.pipe with higher level Eio process lib
     but need Eio_linux.FD for Eio_linux.Low_level.Process.spawn *)
  let create_pipe () =
    let read, write = Unix.pipe () in
    Eio_linux.FD.of_unix ~sw ~seekable:false ~close_unix:false read,
    Eio_linux.FD.of_unix ~sw ~seekable:false ~close_unix:false write
  in
  let readInpipe, writeInpipe = create_pipe () in
  let readOutpipe, writeOutpipe = create_pipe () in
  let cwd = Eio_linux.Low_level.Process.Cwd.Path env#cwd in
  let shell = Eio_linux.Low_level.Process.spawn ~sw ~cwd ~stdout:writeInpipe ~stdin:readOutpipe ~stderr:writeInpipe "/run/current-system/sw/bin/bash" [ "-bash" ] in
  client ~stdout:(Eio.Stdenv.stdout env) ~stdin:(Eio.Stdenv.stdin env) ~write:writeOutpipe ~read:readInpipe;
  ignore(Eio_linux.Low_level.Process.wait shell)
