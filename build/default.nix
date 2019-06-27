{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; # for "wasm"
                              }
, compiler ? "ghc864"
}:
with nixpkgs;
let
  haskellPackages_ = pkgs.haskell.packages.${compiler}.extend (self: super: rec {
    wasm                    = self.callPackage ./cabal2nix/wasm.nix {};
    language-powerquery-ast = self.callPackage ./cabal2nix/language-powerquery-ast.nix {};
    language-powerquery     = self.callPackage ./cabal2nix/language-powerquery.nix {};
    pbix                    = self.callPackage ./cabal2nix/pbix.nix {};
    m2wasm                  = self.callPackage ./cabal2nix/m2wasm.nix {};
  });

  haskellEnv = haskellPackages_.ghcWithPackages (ps: with ps; [
    language-powerquery-ast
    language-powerquery
    pbix
    m2wasm
  ]);

in
stdenv.mkDerivation rec {
  name = "language-powerquery";

  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    haskellEnv
  ];
}
