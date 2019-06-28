NIX_FLAGS=--cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz

build:
	nix-build ${NIX_FLAGS}

build-js:
	nix-build ${NIX_FLAGS} --arg compiler \"ghcjs\"

shell:
	nix-shell ${NIX_FLAGS}

notebook:
	nix-shell ${NIX_FLAGS} -A env notebook.nix --command "jupyter lab --notebook-dir=../examples/"

docker:
	nix-build ${NIX_FLAGS} docker.nix -o docker.tar.gz
