let read_socket ~sw:_ event_socket =
  let buf = Eio.Buf_read.of_flow ~max_size:max_int event_socket in
  while true do
    let msg = Eio.Buf_read.line buf in
    match Event.parse msg with
    | Some event -> Handler.on_event event
    | None -> ()
  done

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let event_sockaddr = Socket.make Event.socket_name in
  let event_socket = Eio.Net.connect ~sw env#net event_sockaddr in
  read_socket ~sw event_socket
