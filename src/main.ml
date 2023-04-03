
let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  (* Could use Eio_unix.pipe with higher level Eio process lib
     but need Eio_linux.FD for Eio_linux.Low_level.Process.spawn *)
  let child =
    let action = Eio_posix.Low_level.Process.Fork_action.execve
      "/run/current-system/sw/bin/bash"
      ~argv:[| "-bash" |]
      ~env:[||] in
    Eio_linux.Low_level.Process.spawn ~sw [ action ] in
  ignore @@ Eio.Promise.await (Eio_linux.Low_level.Process.exit_status child);;
