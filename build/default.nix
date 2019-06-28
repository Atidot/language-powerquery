{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc864"
}:
with nixpkgs;
let
  languagePowerQueryASTSrc = ../language-powerquery-ast;
  languagePowerQuerySrc    = ../language-powerquery;
  pbixSrc                  = ../pbix;
  m2wasmSrc                = ../m2wasm;

  easeIfGHCJS = package:
    if lib.hasPrefix "ghcjs" compiler
    then haskell.lib.dontHaddock (haskell.lib.dontCheck package)
    else package;

  haskellPackages = pkgs.haskell.packages.${compiler}.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
        (self: hspkgs: {
          lens             = easeIfGHCJS hspkgs.lens;
          comonad          = easeIfGHCJS hspkgs.comonad;
          semigroupoids    = easeIfGHCJS hspkgs.semigroupoids;
          QuickCheck       = easeIfGHCJS hspkgs.QuickCheck;
          tasty-quickcheck = easeIfGHCJS hspkgs.tasty-quickcheck;
          scientific       = easeIfGHCJS hspkgs.scientific;
          temporary        = easeIfGHCJS hspkgs.temporary;
          #--
          wasm                    = haskell.lib.dontCheck (haskell.lib.doJailbreak hspkgs.wasm);
          language-powerquery-ast = hspkgs.callCabal2nix "language-powerquery-ast" "${languagePowerQueryASTSrc}" {};
          language-powerquery     = hspkgs.callCabal2nix "language-powerquery" "${languagePowerQuerySrc}" {};
          pbix                    = hspkgs.callCabal2nix "pbix" "${pbixSrc}" {};
          m2wasm                  = hspkgs.callCabal2nix "m2wasm" "${m2wasmSrc}" {};
        });
      });

  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
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
