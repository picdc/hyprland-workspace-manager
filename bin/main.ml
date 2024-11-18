let read_socket ~sw ~env event_socket =
  let buf = Eio.Buf_read.of_flow ~max_size:max_int event_socket in
  while true do
    let msg = Eio.Buf_read.line buf in
    match Event.parse msg with
    | Some event -> Handler.on_event sw env event
    | None -> ()
  done

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Socket.with_connection ~sw ~env Event.socket_name read_socket
