let socket_name = ".socket.sock"

type monitor = { id : int; name : string; desc : string }
type workspace = Wksp of int
type move_workspace = { workspace : workspace; monitor : monitor }

type _ command =
  | Monitors : monitor list command
  | Workspaces : workspace list command
  | MoveWorkspaceToMonitor : move_workspace -> bool command
(* | Seq : 'a command * 'b command -> ('a * 'b) command *)

module Option_syntax = struct
  let ( let* ) x f = Option.bind x f
end

module Json = struct
  let get_field json path transform =
    let open Option_syntax in
    let* value = Ezjsonm.find_opt json path in
    try Some (transform value) with _ -> None
end

let pp_monitor ppf { id; name; desc } =
  Format.fprintf ppf "{ id: %d; name: %s; desc: %s }" id name desc

let pp_monitors = Format.pp_print_list ~pp_sep:Format.pp_print_space pp_monitor

let parse_monitor json =
  let open Option_syntax in
  let* id = Json.get_field json [ "id" ] Ezjsonm.get_int in
  let* name = Json.get_field json [ "name" ] Ezjsonm.get_string in
  let* desc = Json.get_field json [ "description" ] Ezjsonm.get_string in
  Some { id; name; desc }

let parse_monitors = function
  | `A monitors -> Some (List.filter_map parse_monitor monitors)
  | _ -> None

let pp_workspace ppf (Wksp id) = Format.pp_print_int ppf id

let pp_workspaces =
  Format.pp_print_list ~pp_sep:Format.pp_print_space pp_workspace

let parse_workspace json =
  let open Option_syntax in
  let* id = Json.get_field json [ "id" ] Ezjsonm.get_int in
  Some (Wksp id)

let parse_workspaces = function
  | `A workspace -> Some (List.filter_map parse_workspace workspace)
  | _ -> None

let pp_command : type t. _ -> t command * t -> unit =
 fun ppf -> function
  | Monitors, monitors -> pp_monitors ppf monitors
  | Workspaces, workspaces -> pp_workspaces ppf workspaces
  | MoveWorkspaceToMonitor { workspace; monitor }, b ->
      Format.fprintf ppf "Move %a to %a: %b" pp_workspace workspace pp_monitor
        monitor b
(* | Seq (c1, c2), (res1, res2) -> *)
(*     Format.fprintf ppf "%a; %a" pp_command (c1, res1) pp_command (c2, res2) *)

(* let merge_array_parsers p1 p2 = function `A [ r1; r2 ] -> p1 * p2 | `A [ *)

let as_json k v = k (Ezjsonm.from_string v)

let prepare_command : type res. res command -> string * (string -> res option) =
  function
  | Monitors -> ("j/monitors", as_json parse_monitors)
  | Workspaces -> ("j/workspaces", as_json parse_workspaces)
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
  Eio.traceln "Dispatch: %s" dispatch;
  try
    let* result = Socket.request dispatch_socket dispatch parse in
    Eio.traceln "Result: [%a]" pp_command (command, result);
    Some result
  with _exn -> None

let send_command :
    type cmd_res. sw:_ -> env:_ -> cmd_res command -> cmd_res option =
 fun ~sw ~env command ->
  Socket.with_connection ~sw ~env socket_name (send_command_raw ~command)
