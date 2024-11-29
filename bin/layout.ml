type workspace = {
  id : Commands.workspace;
  monitor : Commands.monitor;
  active : bool;
}

type monitor_kind = Primary | Secondary

let pp_monitor_kind ppf = function
  | Primary -> Format.fprintf ppf "primary"
  | Secondary -> Format.fprintf ppf "secondary"

let monitor_kind_of_string = function
  | "primary" -> Some Primary
  | "secondary" -> Some Secondary
  | _ -> None

let read_monitor stdout stdin kind monitors =
  let find_monitor i = List.find_opt (fun m -> m.Commands.id = i) monitors in
  Eio.Flow.copy_string
    (Format.asprintf "Please select %a monitor (by ID%s): " pp_monitor_kind kind
       (if kind = Secondary then " or `none` to use primary only" else ""))
    stdout;
  let selection = Eio.Buf_read.(of_flow stdin ~max_size:max_int |> line) in
  if selection = "none" && kind = Secondary then None
  else
    match int_of_string_opt selection with
    | None -> failwith "Please enter a valid monitor id"
    | Some i -> (
        match find_monitor i with
        | None -> failwith "Please enter a valid monitor id"
        | Some m -> Some m)

let interactive_select_monitors monitors ~env =
  let stdout = Eio.Stdenv.stdout env in
  let stdin = Eio.Stdenv.stdin env in
  Eio.Flow.copy_string
    (Format.asprintf "Monitors: %a\n" Commands.pp_monitors monitors)
    stdout;
  let primary =
    match read_monitor stdout stdin Primary monitors with
    | None -> assert false
    | Some m -> m
  in
  let secondary =
    read_monitor stdout stdin Secondary monitors
    |> Option.value ~default:primary
  in
  (primary, secondary)

let assign ~env ~interactive monitors workspaces =
  let primary, secondary =
    if interactive then interactive_select_monitors monitors ~env
    else
      match
        List.sort (fun m1 m2 -> Int.compare m1.Commands.id m2.id) monitors
      with
      | [] -> failwith "No monitor found"
      | m :: [] -> (m, m)
      | m1 :: m2 :: _ -> (m1, m2)
  in
  List.init 9 (fun id ->
      let id = id + 1 in
      let monitor = if id mod 2 <> 0 then secondary else primary in
      let active = List.mem (Commands.Wksp id) workspaces in
      { id = Commands.Wksp id; monitor; active })

let to_conf_line workspace =
  Format.asprintf "workspace = %a, monitor:desc:%s\n" Commands.pp_workspace
    workspace.id workspace.monitor.desc

let to_conf_file ~env assignments =
  let home = Sys.getenv "HOME" in
  let path =
    Eio.Path.(env#fs / home / ".config" / "hypr" / "workspace_manager.conf")
  in
  Eio.Path.with_open_out ~append:false ~create:(`Or_truncate 0o644) path
    (fun file ->
      List.iter
        (fun assignment ->
          let line = to_conf_line assignment in
          Eio.Flow.write file [ Cstruct.of_string line ])
        assignments)
