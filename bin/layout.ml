open Utils
open Resources

type workspace = { id : Workspace.t; monitor : Monitor.t; active : bool }
type monitor_kind = Primary | Secondary

exception Invalid_monitor_kind of Json.path

let monitor_kind_to_string = function
  | Primary -> "primary"
  | Secondary -> "secondary"

let pp_monitor_kind ppf kind =
  Format.fprintf ppf "%s" @@ monitor_kind_to_string kind

let monitor_kind_of_string ?(field_loc = []) = function
  | "primary" -> Primary
  | "secondary" -> Secondary
  | _ -> raise (Invalid_monitor_kind field_loc)

(* TODO: handle the errors, for now this is... unsatisfying. *)
module Configuration = struct
  type assignment = monitor_kind * Workspace.t list

  let encode_assigment (kind, workpaces) =
    `O
      [
        ("monitor", `String (monitor_kind_to_string kind));
        ( "workpaces",
          `A
            (List.map
               (fun (Workspace.Wksp i) -> `Float (float_of_int i))
               workpaces) );
      ]

  let decode_assignment ?field_loc json =
    let monitor_json =
      Json.get_field ?field_loc json [ "monitor" ] Json.get_string
    in
    let monitor = monitor_kind_of_string ?field_loc monitor_json in
    let workspaces =
      Json.get_field ?field_loc json [ "workspaces" ] Json.get_ints
    in
    (monitor, List.map (fun i -> Workspace.Wksp i) workspaces)

  type layout = assignment list

  let encode_layout l = `A (List.map encode_assigment l)

  let decode_layout ?field_loc json =
    Json.get_list ?field_loc decode_assignment json

  let default_layout =
    [
      (Primary, [ Workspace.Wksp 2; Wksp 4; Wksp 6; Wksp 8 ]);
      (Secondary, [ Wksp 1; Wksp 3; Wksp 5; Wksp 7; Wksp 9 ]);
    ]

  type monitor_whitelist = Monitor.desc list

  let encode_monitor_whitelist l = Ezjsonm.strings l

  let decode_monitor_whitelist ?field_loc json =
    Json.get_strings ?field_loc json

  type t = {
    primary_monitors : monitor_whitelist;
    secondary_monitors : monitor_whitelist;
    layout : layout;
  }

  let encode conf =
    `O
      [
        ( monitor_kind_to_string Primary,
          encode_monitor_whitelist conf.primary_monitors );
        ( monitor_kind_to_string Secondary,
          encode_monitor_whitelist conf.secondary_monitors );
        ("layout", `A (List.map encode_assigment conf.layout));
      ]

  let decode json =
    let primary_monitors =
      Json.get_field json
        [ monitor_kind_to_string Primary ]
        decode_monitor_whitelist
    in
    let secondary_monitors =
      Json.get_field json
        [ monitor_kind_to_string Secondary ]
        decode_monitor_whitelist
    in
    let layout = Json.get_field json [ "layout" ] decode_layout in
    { primary_monitors; secondary_monitors; layout }

  let default =
    { primary_monitors = []; secondary_monitors = []; layout = default_layout }

  let read_or_default env path =
    let result =
      try
        let file = Eio.Path.with_open_in path Eio.Flow.read_all in
        Some (Ezjsonm.from_string file |> decode)
      with _ ->
        Logs.pp_logs env Error "Couldn't read layout file at %a\n%!" Eio.Path.pp
          path;
        None
    in
    Option.value ~default result
end

let read_monitor stdout stdin kind monitors =
  let find_monitor i = List.find_opt (fun m -> m.Monitor.id = i) monitors in
  Eio_format.printf stdout "Please select %a monitor (by ID%s): "
    pp_monitor_kind kind
    (if kind = Secondary then " or `none` to use primary only" else "");
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
  let stdout = Eio.Stdenv.stdout env.Env.env in
  let stdin = Eio.Stdenv.stdin env.env in
  Eio_format.printf stdout "Available monitors: %a\n%!" Monitor.pp_list monitors;
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

let select_monitors monitors conf =
  let open Option_syntax in
  let rec select whitelisted =
    match whitelisted with
    | [] -> None
    | m :: rem -> (
        match List.find_opt (fun m' -> m'.Monitor.desc = m) monitors with
        | Some m -> Some m
        | None -> select rem)
  in
  let* primary = select conf.Configuration.primary_monitors in
  let secondary =
    select (List.filter (fun m -> m <> primary.desc) conf.secondary_monitors)
    |> Option.value ~default:primary
  in
  Some (primary, secondary)

let select_arbitrary_monitors monitors =
  match List.sort (fun m1 m2 -> Int.compare m1.Monitor.id m2.id) monitors with
  | [] -> failwith "No monitor found"
  | m :: [] -> (m, m)
  | m1 :: m2 :: _ -> (m1, m2)

let assign ~env ~interactive monitors current_workspaces configuration =
  let primary, secondary =
    if interactive then interactive_select_monitors monitors ~env
    else
      match select_monitors monitors configuration with
      | None -> select_arbitrary_monitors monitors
      | Some res -> res
  in
  Logs.pp_logs env Debug "Primary monitor: %a, secondary monitor: %a\n%!"
    Monitor.pp primary Monitor.pp secondary;
  List.fold_left
    (fun workspaces (monitor, wksp_ids) ->
      let monitor =
        match monitor with Primary -> primary | Secondary -> secondary
      in
      List.fold_left
        (fun workspaces (Workspace.Wksp id as wksp) ->
          let active = List.mem (Workspace.Wksp id) current_workspaces in
          Logs.pp_logs env Debug "Assigning workspace %a to monitor %a\n%!"
            Workspace.pp wksp Monitor.pp monitor;
          { id = Workspace.Wksp id; monitor; active } :: workspaces)
        workspaces wksp_ids)
    [] configuration.layout

let to_conf_line workspace =
  Format.asprintf "workspace = %a, monitor:desc:%s\n" Workspace.pp workspace.id
    workspace.monitor.desc

let to_conf_file ~env assignments =
  let path = Resources.Unix_env.workspaces_configuration env.Env.env in
  Eio.Path.with_open_out ~append:false ~create:(`Or_truncate 0o644) path
    (fun file ->
      Logs.pp_logs env Debug "Writing configuration to %a\n%!" Eio.Path.pp path;
      List.iter
        (fun assignment ->
          let line = to_conf_line assignment in
          Eio.Flow.write file [ Cstruct.of_string line ])
        assignments)
