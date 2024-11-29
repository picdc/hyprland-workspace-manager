open Utils

module Monitor = struct
  type t = { id : int; name : string; desc : string }

  let pp ppf { id; name; desc } =
    Format.fprintf ppf "{ id: %d; name: %s; desc: %s }" id name desc

  let pp_list = Format.pp_print_list ~pp_sep:Format.pp_print_space pp

  let parse json =
    let open Option_syntax in
    let* id = Json.get_field json [ "id" ] Ezjsonm.get_int in
    let* name = Json.get_field json [ "name" ] Ezjsonm.get_string in
    let* desc = Json.get_field json [ "description" ] Ezjsonm.get_string in
    Some { id; name; desc }

  let parse_array = function
    | `A monitors -> Some (List.filter_map parse monitors)
    | _ -> None
end

module Workspace = struct
  type t = Wksp of int

  let pp ppf (Wksp id) = Format.pp_print_int ppf id
  let pp_list = Format.pp_print_list ~pp_sep:Format.pp_print_space pp

  let parse json =
    let open Option_syntax in
    let* id = Json.get_field json [ "id" ] Ezjsonm.get_int in
    Some (Wksp id)

  let parse_array = function
    | `A workspace -> Some (List.filter_map parse workspace)
    | _ -> None
end
