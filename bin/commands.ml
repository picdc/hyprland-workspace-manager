let socket_name = ".socket.sock"

type monitor = { id : int; name : string; desc : string }
type _ command = Monitors : monitor list command

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

let send_command_raw :
    type cmd_res.
    sw:_ -> env:_ -> command:cmd_res command -> _ -> cmd_res option =
 fun ~sw:_ ~env:_ ~command dispatch_socket ->
  match command with
  | Monitors -> (
      (* Eio.traceln "Socket: %a" (Eio.Net.getaddrinfo_stream socket); *)
      try
        Eio.Flow.write dispatch_socket [ Cstruct.of_string "j/monitors" ];
        Eio.Flow.shutdown dispatch_socket `Send;
        let buf = Eio.Buf_read.of_flow ~max_size:max_int dispatch_socket in
        let rsp = Eio.Buf_read.take_all buf in
        let monitors = parse_monitors (Ezjsonm.from_string rsp) in
        Eio.traceln "Monitors: [%a]"
          (Format.pp_print_option (Format.pp_print_list pp_monitor))
          monitors;
        monitors
      with _exn -> None)

let send_command :
    type cmd_res. sw:_ -> env:_ -> cmd_res command -> cmd_res option =
 fun ~sw ~env command ->
  Socket.with_connection ~sw ~env socket_name (send_command_raw ~command)
