type workspace = {
  id : Commands.workspace;
  monitor : Commands.monitor;
  active : bool;
}

let assign monitors workspaces =
  let m1, m2 =
    match monitors with
    | [] -> failwith "No monitor found"
    | m :: [] -> (m, m)
    | m1 :: m2 :: _ -> (m1, m2)
  in
  List.init 9 (fun id ->
      let id = id + 1 in
      let monitor = if id mod 2 = 0 then m1 else m2 in
      let active = List.mem id workspaces in
      { id = Commands.Wksp id; monitor; active })

let to_conf_line workspace =
  Format.printf "workspace = %a, monitor:%s" Commands.pp_workspace workspace.id
    workspace.monitor.name
