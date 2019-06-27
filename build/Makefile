build:
	nix-build

shell:
	nix-shell

docker:
	nix-build docker.nix -o docker.tar.gz

cabal2nix:
	cabal2nix ../language-powerquery-ast > ./cabal2nix/language-powerquery-ast.nix
	cabal2nix ../language-powerquery     > ./cabal2nix/language-powerquery.nix
	cabal2nix ../pbix                    > ./cabal2nix/pbix.nix
	cabal2nix ../m2wasm                  > ./cabal2nix/m2wasm.nix
	cabal2nix --jailbreak --no-check --no-hpack https://github.com/SPY/haskell-wasm > ./cabal2nix/wasm.nix
