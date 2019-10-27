{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc864"
}:
with nixpkgs;
let
  jupyterWith = builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "097591d0949ebf3645d258c5b1c03dcc59a7afcf";
  };
  nixpkgsPath = jupyterWith + "/nix";
  pkgs = import nixpkgsPath {};

  haskell = import ./haskell.nix
                   { nixpkgs = pkgs;
                     inherit compiler;
                     haskellPackages = pkgs.haskellPackages;
                   };
  haskellPackages' = haskell.packages;
  ease = haskell.ease;

  haskellPackages = haskellPackages'.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
      (self: hspkgs: {
        });
  });

  jupyter = import jupyterWith { pkgs=pkgs; };

  ihaskellWithPackages = jupyter.kernels.iHaskellWith {
    #extraIHaskellFlags = "--debug";
    haskellPackages=haskellPackages;
    name = "platform-env-ihaskell";
    packages = p: with p; [
      lens
      lens-aeson
      language-powerquery-ast
      language-powerquery
      pbix
    ];
  };

  jupyterEnvironment = jupyter.jupyterlabWith {
    kernels = [ ihaskellWithPackages ];
  };
in
jupyterEnvironment
