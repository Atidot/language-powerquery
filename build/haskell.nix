{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc844"
, haskellPackages ? nixpkgs.haskell.packages.${compiler}
}:
with nixpkgs;
let
  isGHCJS = lib.hasPrefix "ghcjs" compiler;
  ease = package: haskell.lib.doJailbreak (haskell.lib.dontHaddock (haskell.lib.dontCheck package));

  #----
  fixesGHCJS = hspkgs: if isGHCJS then {
    lens             = ease hspkgs.lens;
    comonad          = ease hspkgs.comonad;
    semigroupoids    = ease hspkgs.semigroupoids;
    QuickCheck       = ease hspkgs.QuickCheck;
    tasty-quickcheck = ease hspkgs.tasty-quickcheck;
    scientific       = ease hspkgs.scientific;
    temporary        = ease hspkgs.temporary;
  } else {};

  #----
  languagePowerQueryASTSrc = ../language-powerquery-ast;
  languagePowerQuerySrc    = ../language-powerquery;
  pbixSrc                  = ../pbix;
  m2wasmSrc                = ../m2wasm;

  projectPackages = hspkgs: {
    system-fileio           = ease hspkgs.system-fileio;
    wasm                    = ease hspkgs.wasm;
    language-powerquery-ast = hspkgs.callCabal2nix "language-powerquery-ast" "${languagePowerQueryASTSrc}" {};
    language-powerquery     = hspkgs.callCabal2nix "language-powerquery" "${languagePowerQuerySrc}" {};
    pbix                    = hspkgs.callCabal2nix "pbix" "${pbixSrc}" {};
    m2wasm                  = hspkgs.callCabal2nix "m2wasm" "${m2wasmSrc}" {};
  };
in
haskellPackages.override (old: {
  overrides = pkgs.lib.composeExtensions old.overrides
    (self: hspkgs:
      fixesGHCJS hspkgs
   // projectPackages hspkgs
    );
})
