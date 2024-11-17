let xdg_runtime_dir () = Sys.getenv "XDG_RUNTIME_DIR"
let hyprland_instance_signature () = Sys.getenv "HYPRLAND_INSTANCE_SIGNATURE"

let make socket_name =
  let ( // ) = Filename.concat in
  `Unix
    (xdg_runtime_dir () // "hypr"
    // hyprland_instance_signature ()
    // socket_name)
