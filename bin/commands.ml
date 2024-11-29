open Utils
open Resources

let socket_name = ".socket.sock"

type move_workspace = { workspace : Workspace.t; monitor : Monitor.t }

type _ command =
  | Monitors : Monitor.t list command
  | Workspaces : Workspace.t list command
  | MoveWorkspaceToMonitor : move_workspace -> bool command
(* | Seq : 'a command * 'b command -> ('a * 'b) command *)

let pp_command : type t. _ -> t command * t -> unit =
 fun ppf -> function
  | Monitors, monitors -> Monitor.pp_list ppf monitors
  | Workspaces, workspaces -> Workspace.pp_list ppf workspaces
  | MoveWorkspaceToMonitor { workspace; monitor }, b ->
      Format.fprintf ppf "Move %a to %a: %b" Workspace.pp workspace Monitor.pp
        monitor b
(* | Seq (c1, c2), (res1, res2) -> *)
(*     Format.fprintf ppf "%a; %a" pp_command (c1, res1) pp_command (c2, res2) *)

(* let merge_array_parsers p1 p2 = function `A [ r1; r2 ] -> p1 * p2 | `A [ *)

let as_json k v = k (Ezjsonm.from_string v)

let prepare_command : type res. res command -> string * (string -> res option) =
  function
  | Monitors -> ("j/monitors", as_json Monitor.parse_array)
  | Workspaces -> ("j/workspaces", as_json Workspace.parse_array)
  | MoveWorkspaceToMonitor { workspace = Wksp id; monitor; _ } ->
      ( Format.sprintf "j/dispatch moveworkspacetomonitor %d %s" id monitor.name,
        fun r -> Some (r = "ok") )
(* | Seq (c1, (Seq (_, _) as subseq)) -> ( *)
(*     let c1_dispatch, c1_parser = prepare_command c1 in *)
(*     let subseq_dispatch, subseq_parser = prepare_command subseq in *)
(*     ( "[[BATCH]]" ^ c1_dispatch ^ ";" ^ subseq_dispatch, *)
(*       function *)
(*       | `A (v1 :: (_ :: _ :: _ as subarray)) -> *)
(*           let* r1 = c1_parser v1 in *)
(*           let* r2 = subseq_parser (`A subarray) in *)
(*           Some (r1, r2) *)
(*       | _ -> None )) *)
(* | Seq (c1, c2) -> ( *)
(*     let c1_dispatch, c1_parser = prepare_command c1 in *)
(*     let c2_dispatch, c2_parser = prepare_command c2 in *)
(*     ( c1_dispatch ^ ";" ^ c2_dispatch, *)
(*       function *)
(*       | `A [ v1; v2 ] -> *)
(*           let* r1 = c1_parser v1 in *)
(*           let* r2 = c2_parser v2 in *)
(*           Some (r1, r2) *)
(*       | _ -> None )) *)

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
