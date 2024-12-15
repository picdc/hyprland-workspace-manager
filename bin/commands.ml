open Utils
open Resources

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
    type cmd_res. env:Env.t -> command:cmd_res command -> _ -> cmd_res option =
 fun ~env ~command dispatch_socket ->
  let open Option_syntax in
  let dispatch, parse = prepare_command command in
  Logs.pp_logs env Debug "Sending command: [%s]\n%!" dispatch;
  try
    let* result = Socket.request dispatch_socket dispatch parse in
    Logs.pp_logs env Debug "Result: %a\n%!" pp_command (command, result);
    Some result
  with _exn -> None

let send_command : type cmd_res. env:Env.t -> cmd_res command -> cmd_res option
    =
 fun ~env command ->
  Socket.with_connection ~env Unix_env.hyprland_dispatch_socket
    (send_command_raw ~command)
