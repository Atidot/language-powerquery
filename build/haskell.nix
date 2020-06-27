{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc883"
, haskellPackages ? nixpkgs.haskell.packages.${compiler}
}:
with nixpkgs;
rec {
  ease = package: with haskell.lib;
    ( doJailbreak
    ( dontHaddock
    ( dontCheck
    ( package
    ))));
  languagePowerQueryASTSrc    = ../language-powerquery-ast;
  languagePowerQuerySrc       = ../language-powerquery;
  languagePowerQueryEditorSrc = ../language-powerquery-editor;
  pbixSrc                     = ../pbix;

  projectPackages = hspkgs: {
    language-powerquery-ast    = hspkgs.callCabal2nix "language-powerquery-ast"    "${languagePowerQueryASTSrc}" {};
    language-powerquery        = hspkgs.callCabal2nix "language-powerquery"        "${languagePowerQuerySrc}" {};
    language-powerquery-editor = hspkgs.callCabal2nix "language-powerquery-editor" "${languagePowerQueryEditorSrc}" {};
    pbix                       = hspkgs.callCabal2nix "pbix"                       "${pbixSrc}" {};
  };

  packages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
      (self: hspkgs:
        projectPackages hspkgs
      );
  });
}
