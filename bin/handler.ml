let dispatch_workspaces ~sw ~env ~interactive ~overwrite () =
  let open Utils.Option_syntax in
  let monitors, workspaces =
    Eio.Fiber.pair
      (fun () -> Commands.send_command ~sw ~env Commands.Monitors)
      (fun () -> Commands.send_command ~sw ~env Commands.Workspaces)
  in
  let* monitors = monitors in
  let* workspaces = workspaces in
  let workspaces = Layout.assign ~env ~interactive monitors workspaces in
  let commands =
    List.filter_map
      (fun assignment ->
        if assignment.Layout.active then
          Some
            (fun () ->
              Commands.send_command ~sw ~env
                (Commands.MoveWorkspaceToMonitor
                   { workspace = assignment.id; monitor = assignment.monitor })
              |> ignore)
        else None)
      workspaces
  in
  if overwrite then Layout.to_conf_file ~env workspaces;
  let () = Eio.Fiber.all commands in
  Some ()

let on_monitor_change sw env =
  dispatch_workspaces ~sw ~env ~interactive:false ~overwrite:true ()

let on_event sw env ev =
  match ev.Event.event with
  | Event.MonitorAdded | MonitorAddedV2 | MonitorRemoved ->
      on_monitor_change sw env |> ignore
  | _ -> ()
