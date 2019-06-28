{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; # for "wasm"
                              }
, compiler ? "ghc864"
}:
with nixpkgs;
let
  easeIfGHCJS = package:
    if lib.hasPrefix "ghcjs" compiler
    then haskell.lib.dontHaddock (haskell.lib.dontCheck package)
    else package;

  haskellPackages_ = pkgs.haskell.packages.${compiler}.extend (self: super: rec {
    lens             = easeIfGHCJS super.lens;
    comonad          = easeIfGHCJS super.comonad;
    semigroupoids    = easeIfGHCJS super.semigroupoids;
    QuickCheck       = easeIfGHCJS super.QuickCheck;
    tasty-quickcheck = easeIfGHCJS super.tasty-quickcheck;
    scientific       = easeIfGHCJS super.scientific;
    temporary        = easeIfGHCJS super.temporary;
    #--
    wasm                    = self.callPackage ./wasm.nix {};
    language-powerquery-ast = self.callPackage ./language-powerquery-ast.nix {};
    language-powerquery     = self.callPackage ./language-powerquery.nix {};
    pbix                    = self.callPackage ./pbix.nix {};
    m2wasm                  = self.callPackage ./m2wasm.nix {};
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
