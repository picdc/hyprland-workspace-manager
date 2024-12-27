module Option_syntax = struct
  let ( let* ) x f = Option.bind x f
end

module Json = struct
  type path = string list

  (* Paths are used to give an approximate location of the error, they are not
     precise under arrays *)
  type parse_error =
    | Cannot_find_field of path
    | Not_an_array of path
    | Not_an_integer of path
    | Not_a_string of path

  exception Parsing of parse_error

  let parse_array ?(field_loc = [])
      (parse : ?field_loc:path -> Ezjsonm.value -> 'a) = function
    | `A array -> List.map (parse ~field_loc) array
    | _ -> raise (Parsing (Not_an_array field_loc))

  let get_int ?(field_loc = []) = function
    | `Float f -> (
        try int_of_float f
        with _ -> raise (Parsing (Not_an_integer field_loc)))
    | _ -> raise (Parsing (Not_an_integer field_loc))

  let get_ints ?(field_loc = []) = parse_array ~field_loc get_int

  let get_string ?(field_loc = []) : Ezjsonm.value -> 'a = function
    | `String s -> s
    | _ -> raise (Parsing (Not_a_string field_loc))

  let get_strings ?(field_loc = []) = parse_array ~field_loc get_string
  let get_list ?(field_loc = []) get = parse_array ~field_loc get

  let get_field ?(field_loc = []) json path
      (transform : ?field_loc:path -> Ezjsonm.value -> 'a) =
    let full_field_loc = field_loc @ path in
    match Ezjsonm.find_opt json path with
    | Some value -> transform ~field_loc:full_field_loc value
    | None -> raise (Parsing (Cannot_find_field full_field_loc))
end

module Eio_format = struct
  let printf sink fmt =
    Format.kasprintf (fun s -> Eio.Flow.copy_string s sink) fmt
end
