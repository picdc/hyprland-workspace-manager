let on_event sw env ev =
  match ev.Event.event with
  | Event.MonitorAdded | MonitorAddedV2 | MonitorRemoved ->
      Eio.traceln "Monitor changed: %s >> %s" (Event.to_string ev.event) ev.data;
      let _monitors, _workspaces =
        Eio.Fiber.pair
          (fun () -> Commands.send_command ~sw ~env Commands.Monitors)
          (fun () -> Commands.send_command ~sw ~env Commands.Workspaces)
      in
      ()
  | _ -> ()
