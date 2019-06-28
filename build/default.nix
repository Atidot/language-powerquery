{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc844"
}:
with nixpkgs;
let
  haskellPackages = import ./haskell.nix { inherit nixpkgs compiler; };

  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
    language-powerquery-ast
    language-powerquery
    pbix
    m2wasm
  ]);

in
stdenv.mkDerivation rec {
  name = "powerquery-env";

  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    haskellEnv
  ];
}
