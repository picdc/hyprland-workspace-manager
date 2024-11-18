let socket_name = ".socket.sock"

type monitor = { id : int; name : string; desc : string }
type workspace = Wksp of int

type _ command =
  | Monitors : monitor list command
  | Workspaces : workspace list command

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

let parse_monitor json =
  let open Option_syntax in
  let* id = Json.get_field json [ "id" ] Ezjsonm.get_int in
  let* name = Json.get_field json [ "name" ] Ezjsonm.get_string in
  let* desc = Json.get_field json [ "description" ] Ezjsonm.get_string in
  Some { id; name; desc }

let parse_monitors = function
  | `A monitors -> Some (List.filter_map parse_monitor monitors)
  | _ -> None

let parse_workspace json =
  let open Option_syntax in
  let* id = Json.get_field json [ "id" ] Ezjsonm.get_int in
  Some (Wksp id)

let pp_workspace ppf (Wksp id) = Format.pp_print_int ppf id

let parse_workspaces = function
  | `A workspace -> Some (List.filter_map parse_workspace workspace)
  | _ -> None

let send_command_raw :
    type cmd_res.
    sw:_ -> env:_ -> command:cmd_res command -> _ -> cmd_res option =
 fun ~sw:_ ~env:_ ~command dispatch_socket ->
  match command with
  | Monitors -> (
      (* Eio.traceln "Socket: %a" (Eio.Net.getaddrinfo_stream socket); *)
      try
        let monitors =
          Socket.request_json dispatch_socket "j/monitors" parse_monitors
        in
        Eio.traceln "Monitors: [%a]"
          (Format.pp_print_option
             (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_monitor))
          monitors;
        monitors
      with _exn -> None)
  | Workspaces -> (
      (* Eio.traceln "Socket: %a" (Eio.Net.getaddrinfo_stream socket); *)
      try
        let workspaces =
          Socket.request_json dispatch_socket "j/workspaces" parse_workspaces
        in
        Eio.traceln "Workspaces: [%a]"
          (Format.pp_print_option
             (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_workspace))
          workspaces;
        workspaces
      with _exn -> None)

let send_command :
    type cmd_res. sw:_ -> env:_ -> cmd_res command -> cmd_res option =
 fun ~sw ~env command ->
  Socket.with_connection ~sw ~env socket_name (send_command_raw ~command)
