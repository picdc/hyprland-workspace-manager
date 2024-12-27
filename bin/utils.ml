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

  let pp_path ppf = function
    | [] -> Format.fprintf ppf "%%root%%"
    | path ->
        Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ".")
          Format.pp_print_string ppf path

  let pp_error ppf = function
    | Cannot_find_field path ->
        Format.fprintf ppf "Field at path `%a` not found" pp_path path
    | Not_an_array path ->
        Format.fprintf ppf "Path `%a` is expected to be an array" pp_path path
    | Not_an_integer path ->
        Format.fprintf ppf "Path `%a` is expected to be an integer" pp_path path
    | Not_a_string path ->
        Format.fprintf ppf "Path `%a` is expected to be a string" pp_path path
end

module Eio_format = struct
  let printf sink fmt =
    Format.kasprintf (fun s -> Eio.Flow.copy_string s sink) fmt
end
