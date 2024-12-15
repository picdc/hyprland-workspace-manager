let socket_name = ".socket2.sock"

type event =
  | MonitorAdded
  | MonitorAddedV2
  | MonitorRemoved
  | Unhandled of string

let of_string = function
  | "monitoradded" -> MonitorAdded
  | "monitoraddedv2" -> MonitorAddedV2
  | "monitorremoved" -> MonitorRemoved
  | ev -> Unhandled ev

let to_string = function
  | MonitorAdded -> "monitoradded"
  | MonitorAddedV2 -> "monitoraddedv2"
  | MonitorRemoved -> "monitorremoved"
  | Unhandled ev -> "Unhandled:" ^ ev

type t = { event : event; data : string }

let pp ppf { event; data } =
  Format.fprintf ppf "{ event: %s; data: %s }" (to_string event) data

let parse event_line =
  let re = Re.Pcre.re "^(\\w+)>>(.+)$" |> Re.compile in
  match Re.all re event_line with
  | [ matched ] ->
      let event = Re.Group.get matched 1 |> of_string in
      let data = Re.Group.get matched 2 in
      Some { event; data }
  | _ ->
      Eio.traceln "Unknown event: %s" event_line;
      None
