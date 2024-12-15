type verbosity = Error | Notice | Debug
type t = { env : Eio_unix.Stdenv.base; verbosity : verbosity }

let verbosity_of_int = function
  | 0 -> Some Error
  | 1 -> Some Notice
  | 2 -> Some Debug
  | _ -> None

let int_of_verbosity = function Error -> 0 | Notice -> 1 | Debug -> 2
let default_verbosity = Error
let default_verbosity_int = int_of_verbosity default_verbosity
