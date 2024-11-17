let on_event ev =
  match ev.Event.event with
  | Event.MonitorAdded | MonitorAddedV2 | MonitorRemoved ->
      Eio.traceln "Monitor changed: %s >> %s" (Event.to_string ev.event) ev.data
  | _ -> ()
