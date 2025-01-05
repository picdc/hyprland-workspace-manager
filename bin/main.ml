open Cmdliner

let daemon ~env event_socket =
  Logs.pp_logs env Notice "Start daemon\n%!";
  let buf = Eio.Buf_read.of_flow ~max_size:max_int event_socket in
  while true do
    let msg = Eio.Buf_read.line buf in
    Logs.pp_logs env Debug "Received event: %S\n%!" msg;
    match Event.parse env msg with
    | Some event ->
        Logs.pp_logs env Debug "Parsed event: %a\n%!" Event.pp event;
        Handler.on_event env event
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
  type options = { verbosity : Env.verbosity }

  let verbosity_flag =
    Arg.(value & opt int Env.default_verbosity_int & info [ "v"; "verbosity" ])

  let options =
    let open Term.Syntax in
    let+ verbosity = verbosity_flag in
    {
      verbosity =
        Option.value ~default:Env.default_verbosity
          (Env.verbosity_of_int verbosity);
    }

  let handler ~sw ~env { verbosity } () =
    let env = Env.{ env; verbosity; switch = sw } in
    Socket.with_connection ~sw ~env Resources.Unix_env.hyprland_event_socket
      daemon

  let name = "daemon"
end)

module Dispatch = CmdMaker (struct
  type options = {
    interactive : bool;
    overwrite : bool;
    verbosity : Env.verbosity;
  }

  let interactive_flag = Arg.(value & flag & info [ "i"; "interactive" ])
  let overwrite_flag = Arg.(value & flag & info [ "w"; "overwrite" ])

  let verbosity_flag =
    Arg.(value & opt int Env.default_verbosity_int & info [ "v"; "verbosity" ])

  let options =
    let open Term.Syntax in
    let+ interactive = interactive_flag
    and+ overwrite = overwrite_flag
    and+ verbosity = verbosity_flag in
    {
      interactive;
      overwrite;
      verbosity =
        Option.value ~default:Env.default_verbosity
          (Env.verbosity_of_int verbosity);
    }

  let handler ~sw ~env (options : options) () =
    let env = Env.{ env; verbosity = options.verbosity; switch = sw } in
    Handler.dispatch_workspaces ~env ~interactive:options.interactive
      ~overwrite:options.overwrite ()
    |> ignore

  let name = "dispatch"
end)

let commands : (module CMD) list = [ (module Daemon); (module Dispatch) ]

let commands ~sw ~env =
  let git_commit =
    Eio.Process.parse_out
      (Eio.Stdenv.process_mgr env)
      Eio.Buf_read.line
      [ "git"; "show"; "--oneline"; "-s" ]
  in
  Cmdliner.Cmd.group (Cmd.info "" ~version:git_commit)
  @@ List.map (fun (module CMD : CMD) -> CMD.cmd ~sw ~env) commands

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> exit (Cmd.eval (commands ~sw ~env))
