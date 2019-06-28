NIX_FLAGS=--cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz

build:
	nix-build ${NIX_FLAGS}

build-js:
	nix-build ${NIX_FLAGS} --arg compiler \"ghcjs\"

shell:
	nix-shell ${NIX_FLAGS}

docker:
	nix-build ${NIX_FLAGS} docker.nix -o docker.tar.gz

cabal2nix:
	cabal2nix ../language-powerquery-ast > ./language-powerquery-ast.nix
	cabal2nix ../language-powerquery     > ./language-powerquery.nix
	cabal2nix ../pbix                    > ./pbix.nix
	cabal2nix ../m2wasm                  > ./m2wasm.nix
	cabal2nix --jailbreak --no-check --no-hpack https://github.com/SPY/haskell-wasm > ./wasm.nix
