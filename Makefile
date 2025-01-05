build:
	@dune build

install:
	@cp _build/install/default/bin/hyprland-workspace-manager /usr/local/bin
