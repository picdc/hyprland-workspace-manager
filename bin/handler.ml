let dispatch_workspaces ?ev ~sw ~env () =
  let open Commands.Option_syntax in
  (* This is for debug only *)
  Option.iter
    (fun ev ->
      Eio.traceln "Monitor changed: %s >> %s"
        (Event.to_string ev.Event.event)
        ev.data)
    ev;
  let monitors, workspaces =
    Eio.Fiber.pair
      (fun () -> Commands.send_command ~sw ~env Commands.Monitors)
      (fun () -> Commands.send_command ~sw ~env Commands.Workspaces)
  in
  let* monitors = monitors in
  let* workspaces = workspaces in
  let workspaces = Layout.assign monitors workspaces in
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
  let () = Eio.Fiber.all commands in
  Some ()

let on_monitor_change ?ev sw env = dispatch_workspaces ~sw ~env ?ev ()

let on_event sw env ev =
  match ev.Event.event with
  | Event.MonitorAdded | MonitorAddedV2 | MonitorRemoved ->
      on_monitor_change sw env ~ev |> ignore
  | _ -> ()
