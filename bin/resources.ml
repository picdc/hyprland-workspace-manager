open Utils

module Monitor = struct
  type desc = string
  type t = { id : int; name : string; desc : desc }

  let pp ppf { id; name; desc } =
    Format.fprintf ppf "{ id: %d; name: %s; desc: %s }" id name desc

  let pp_list = Format.pp_print_list ~pp_sep:Format.pp_print_space pp

  let parse ?field_loc json =
    let id = Json.get_field ?field_loc json [ "id" ] Json.get_int in
    let name = Json.get_field ?field_loc json [ "name" ] Json.get_string in
    let desc =
      Json.get_field ?field_loc json [ "description" ] Json.get_string
    in
    { id; name; desc }

  let parse_array ?field_loc = Json.parse_array ?field_loc parse
end

module Workspace = struct
  type t = Wksp of int

  let pp ppf (Wksp id) = Format.pp_print_int ppf id
  let pp_list = Format.pp_print_list ~pp_sep:Format.pp_print_space pp

  let parse ?field_loc json =
    let id = Json.get_field ?field_loc json [ "id" ] Json.get_int in
    Wksp id

  let parse_array ?field_loc = Json.parse_array ?field_loc parse
end

module Unix_env = struct
  let ( // ) = Filename.concat
  let xdg_runtime_dir () = Sys.getenv "XDG_RUNTIME_DIR"
  let hyprland_instance_signature () = Sys.getenv "HYPRLAND_INSTANCE_SIGNATURE"
  let home () = Sys.getenv "HOME"

  let xdg_config_home () =
    match Sys.getenv_opt "XDG_CONFIG_HOME" with
    | Some s -> s
    | None -> home () // ".config"

  let configuration env =
    Eio.Path.(
      env#fs / xdg_config_home () / "hyprland-workspace-manager" / "config.json")

  let workspaces_configuration env =
    Eio.Path.(env#fs / xdg_config_home () / "hypr" / "workspace_manager.conf")

  let hyprland_socket socket_name =
    let ( // ) = Filename.concat in
    `Unix
      (xdg_runtime_dir () // "hypr"
      // hyprland_instance_signature ()
      // socket_name)

  let hyprland_dispatch_socket_name = ".socket.sock"
  let hyprland_dispatch_socket = hyprland_socket hyprland_dispatch_socket_name
  let hyprland_event_socket_name = ".socket2.sock"
  let hyprland_event_socket = hyprland_socket hyprland_event_socket_name
end
