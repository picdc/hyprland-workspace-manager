let with_connection ~env sockaddr k =
  let socket = Eio.Net.connect ~sw:env.Env.switch env.env#net sockaddr in
  Logs.pp_logs env Debug "Connected to socket %a\n%!" Eio.Net.Sockaddr.pp
    sockaddr;
  let res = k ~env socket in
  Logs.pp_logs env Debug "Closing connection to socket %a\n%!"
    Eio.Net.Sockaddr.pp sockaddr;
  Eio.Net.close socket;
  res

let send env socket msg =
  Logs.pp_logs env Debug "Sending message to socket: %S\n%!" msg;
  Eio.Flow.write socket [ Cstruct.of_string msg ];
  Eio.Flow.shutdown socket `Send

let receive_full env socket =
  let buf = Eio.Buf_read.of_flow ~max_size:max_int socket in
  let res = Eio.Buf_read.take_all buf in
  Logs.pp_logs env Debug "Received message from socket:%S\n%!" res;
  Eio.Flow.shutdown socket `Receive;
  res

let request env socket msg k =
  send env socket msg;
  let rsp = receive_full env socket in
  k rsp

let request_json env socket msg k =
  request env socket msg (fun rsp -> k (Ezjsonm.from_string rsp))
