all:
	nix-build -A language-powerquery-ast.components.library
	nix-build -A language-powerquery.components.library
	nix-build -A pbix.components.library
	nix-build -A pbix.components.exes.pbix
