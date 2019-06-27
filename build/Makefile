build:
	nix-build

shell:
	nix-shell

docker:
	nix-build docker.nix -o docker.tar.gz

cabal2nix:
	cabal2nix ../language-powerquery-ast > ./language-powerquery-ast.nix
	cabal2nix ../language-powerquery     > ./language-powerquery.nix
	cabal2nix ../pbix                    > ./pbix.nix
	cabal2nix ../m2wasm                  > ./m2wasm.nix
	cabal2nix --jailbreak --no-check --no-hpack https://github.com/SPY/haskell-wasm > ./wasm.nix
