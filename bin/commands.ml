open Utils
open Resources

let socket_name = ".socket.sock"

type move_workspace = { workspace : Workspace.t; monitor : Monitor.t }

type _ command =
  | Monitors : Monitor.t list command
  | Workspaces : Workspace.t list command
  | MoveWorkspaceToMonitor : move_workspace -> bool command

let pp_command : type t. _ -> t command * t -> unit =
 fun ppf -> function
  | Monitors, monitors -> Monitor.pp_list ppf monitors
  | Workspaces, workspaces -> Workspace.pp_list ppf workspaces
  | MoveWorkspaceToMonitor { workspace; monitor }, b ->
      Format.fprintf ppf "Move %a to %a: %b" Workspace.pp workspace Monitor.pp
        monitor b

let as_json k v = k (Ezjsonm.from_string v)

let prepare_command : type res. res command -> string * (string -> res option) =
  function
  | Monitors -> ("j/monitors", as_json Monitor.parse_array)
  | Workspaces -> ("j/workspaces", as_json Workspace.parse_array)
  | MoveWorkspaceToMonitor { workspace = Wksp id; monitor; _ } ->
      ( Format.sprintf "j/dispatch moveworkspacetomonitor %d %s" id monitor.name,
        fun r -> Some (r = "ok") )

let send_command_raw :
    type cmd_res.
    sw:_ -> env:_ -> command:cmd_res command -> _ -> cmd_res option =
 fun ~sw:_ ~env:_ ~command dispatch_socket ->
  let open Option_syntax in
  let dispatch, parse = prepare_command command in
  try
    let* result = Socket.request dispatch_socket dispatch parse in
    Some result
  with _exn -> None

let send_command :
    type cmd_res. sw:_ -> env:_ -> cmd_res command -> cmd_res option =
 fun ~sw ~env command ->
  Socket.with_connection ~sw ~env socket_name (send_command_raw ~command)
