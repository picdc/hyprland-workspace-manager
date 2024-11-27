module Option_syntax = struct
  let ( let* ) x f = Option.bind x f
end

module Json = struct
  let get_field json path transform =
    let open Option_syntax in
    let* value = Ezjsonm.find_opt json path in
    try Some (transform value) with _ -> None
end