let xdg_runtime_dir () = Sys.getenv "XDG_RUNTIME_DIR"
let hyprland_instance_signature () = Sys.getenv "HYPRLAND_INSTANCE_SIGNATURE"

let make socket_name =
  let ( // ) = Filename.concat in
  `Unix
    (xdg_runtime_dir () // "hypr"
    // hyprland_instance_signature ()
    // socket_name)

let with_connection ~sw ~env socket_name k =
  let sockaddr = make socket_name in
  let socket = Eio.Net.connect ~sw env#net sockaddr in
  let res = k ~sw ~env socket in
  Eio.Net.close socket;
  res
