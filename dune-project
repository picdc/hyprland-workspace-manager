(lang dune 3.16)

(name hyprland-workspace-manager)

(generate_opam_files true)

(source
 (github picdc/hyprland-workspace-manager))

(authors "Pierrick Couderc")

(maintainers "Pierrick Couderc")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name hyprland-workspace-manager)
 (synopsis "Hyprland workspace layout manager")
 (description "A simple Hyprland daemon that can handle workspace layout for a consistent experience on different setup.")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
