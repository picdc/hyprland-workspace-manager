let on_event sw env ev =
  match ev.Event.event with
  | Event.MonitorAdded | MonitorAddedV2 | MonitorRemoved ->
      Eio.traceln "Monitor changed: %s >> %s" (Event.to_string ev.event) ev.data;
      Commands.send_command ~sw ~env Commands.Workspaces |> ignore
  | _ -> ()
