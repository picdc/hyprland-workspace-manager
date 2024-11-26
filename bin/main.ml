open Cmdliner

let daemon ~sw ~env event_socket =
  Eio.traceln "Start daemon";
  let buf = Eio.Buf_read.of_flow ~max_size:max_int event_socket in
  while true do
    let msg = Eio.Buf_read.line buf in
    match Event.parse msg with
    | Some event -> Handler.on_event sw env event
    | None -> ()
  done

module Daemon = struct
  let handler ~sw ~env () =
    Socket.with_connection ~sw ~env Event.socket_name daemon

  let term ~sw ~env = Term.(const (handler ~sw ~env) $ const ())
  let cmd ~sw ~env = Cmd.v (Cmd.info "daemon") (term ~sw ~env)
end

module Dispatch = struct
  let handler ~sw ~env () = Handler.dispatch_workspaces ~sw ~env () |> ignore
  let term ~sw ~env = Term.(const (handler ~sw ~env) $ const ())
  let cmd ~sw ~env = Cmd.v (Cmd.info "dispatch") (term ~sw ~env)
end

let commands ~sw ~env =
  Cmdliner.Cmd.group (Cmd.info "")
    [ Daemon.cmd ~sw ~env; Dispatch.cmd ~sw ~env ]

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> exit (Cmd.eval (commands ~sw ~env))
