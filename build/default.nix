{ nixpkgs  ? import <nixpkgs> {}
, compiler ? "ghc864"
}:
with nixpkgs;
let
  haskellPackages_ = pkgs.haskell.packages.${compiler}.extend (self: super: rec {
    language-powerquery-ast = self.callPackage ./language-powerquery-ast.nix {};
    language-powerquery     = self.callPackage ./language-powerquery.nix {};
    pbix                    = self.callPackage ./pbix.nix {};
  });

  haskellEnv = haskellPackages_.ghcWithPackages (ps: with ps; [
    language-powerquery-ast
    language-powerquery
    pbix
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
