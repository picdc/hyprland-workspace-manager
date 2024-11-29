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

module type CMD_DESC = sig
  type options

  val options : options Term.t

  val handler :
    sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> options -> unit -> unit

  val name : string
end

module type CMD = sig
  val cmd : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> unit Cmd.t
end

module CmdMaker (Command : CMD_DESC) = struct
  let term ~sw ~env =
    Term.(const (Command.handler ~sw ~env) $ Command.options $ const ())

  let cmd ~sw ~env = Cmd.v (Cmd.info Command.name) (term ~sw ~env)
end

module Daemon = CmdMaker (struct
  type options = unit

  let options = Term.const ()

  let handler ~sw ~env () () =
    Socket.with_connection ~sw ~env Event.socket_name daemon

  let name = "daemon"
end)

module Dispatch = CmdMaker (struct
  type options = { interactive : bool; overwrite : bool }

  let interactive_flag = Arg.(value & flag & info [ "i"; "interactive" ])
  let overwrite_flag = Arg.(value & flag & info [ "w"; "overwrite" ])

  let options =
    Term.(
      map (fun (interactive, overwrite) -> { interactive; overwrite })
      @@ product interactive_flag overwrite_flag)

  let handler ~sw ~env options () =
    Handler.dispatch_workspaces ~sw ~env ~interactive:options.interactive
      ~overwrite:options.overwrite ()
    |> ignore

  let name = "dispatch"
end)

let commands : (module CMD) list = [ (module Daemon); (module Dispatch) ]

let commands ~sw ~env =
  Cmdliner.Cmd.group (Cmd.info "")
  @@ List.map (fun (module CMD : CMD) -> CMD.cmd ~sw ~env) commands

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> exit (Cmd.eval (commands ~sw ~env))
