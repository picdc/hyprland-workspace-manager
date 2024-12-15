let with_connection ~env sockaddr k =
  let socket = Eio.Net.connect ~sw:env.Env.switch env.env#net sockaddr in
  let res = k ~env socket in
  Eio.Net.close socket;
  res

let send socket msg =
  Eio.Flow.write socket [ Cstruct.of_string msg ];
  Eio.Flow.shutdown socket `Send

let receive socket =
  let buf = Eio.Buf_read.of_flow ~max_size:max_int socket in
  let res = Eio.Buf_read.take_all buf in
  Eio.Flow.shutdown socket `Receive;
  res

let request socket msg k =
  send socket msg;
  let rsp = receive socket in
  k rsp

let request_json socket msg k =
  request socket msg (fun rsp -> k (Ezjsonm.from_string rsp))
