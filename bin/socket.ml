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

let send socket msg =
  Eio.Flow.write socket [ Cstruct.of_string msg ];
  Eio.Flow.shutdown socket `Send

let receive socket =
  let buf = Eio.Buf_read.of_flow ~max_size:max_int socket in
  Eio.Buf_read.take_all buf

let request socket msg k =
  send socket msg;
  let rsp = receive socket in
  k rsp

let request_json socket msg k =
  request socket msg (fun rsp -> k (Ezjsonm.from_string rsp))
