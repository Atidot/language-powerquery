build:
	nix-build

shell:
	nix-shell

docker:
	nix-build docker.nix -o docker.tar.gz

cabal2nix:
	cabal2nix ../language-powerquery-ast > language-powerquery-ast.nix
	cabal2nix ../language-powerquery     > language-powerquery.nix
