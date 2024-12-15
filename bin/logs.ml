let compare v1 v2 =
  match (v1, v2) with
  | Env.Error, Env.Error -> 0
  | Error, _ -> -1
  | Notice, Notice -> 0
  | Notice, Error -> 1
  | Notice, _ -> -1
  | Debug, Debug -> 0
  | Debug, _ -> 1

let sink_of_verbosity (env : Env.t) = function
  | Env.Notice | Debug -> env.env#stdout
  | Error -> env.env#stderr

let pp_logs (env : Env.t) verbosity fmt =
  if compare env.verbosity verbosity >= 0 then
    Utils.Eio_format.printf (sink_of_verbosity env verbosity) fmt
  else Format.ifprintf Format.std_formatter fmt
