{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghcjs"
}:
with nixpkgs;
let
  haskell = import ./haskell.nix { inherit nixpkgs compiler; };
  ease    = haskell.ease;
  haskellPackages' = haskell.packages;

  haskellPackages = haskellPackages'.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
        (self: hspkgs: {
          comonad          = ease hspkgs.comonad;
          lens             = ease hspkgs.lens;
          semigroupoids    = ease hspkgs.semigroupoids;
          QuickCheck       = ease hspkgs.QuickCheck;
          tasty-quickcheck = ease hspkgs.tasty-quickcheck;
          scientific       = ease hspkgs.scientific;
          temporary        = ease hspkgs.temporary;
          time-compat      = ease hspkgs.time-compat;
        });
  });

  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
    language-powerquery-ast
    language-powerquery
    pbix
  ]);

in
stdenv.mkDerivation rec {
  name = "language-powerquery-js-env";

  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    haskellEnv
  ];
}
