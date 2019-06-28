{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc844"
}:
with nixpkgs;
let
  jupyterWith = builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "1176b9e8d173f2d2789705ad55c7b53a06155e0f";
  };
  nixpkgsPath = jupyterWith + "/nix";
  pkgs = import nixpkgsPath {};

  haskellPackages = import ./haskell.nix
                  { nixpkgs = pkgs;
                    haskellPackages = pkgs.haskellPackages;
                  };

  jupyter = import jupyterWith { pkgs=pkgs; };

  ihaskellWithPackages = jupyter.kernels.iHaskellWith {
    #extraIHaskellFlags = "--debug";
    haskellPackages=haskellPackages;
    name = "powerquery-env-ihaskell";
    packages = p: with p; [
      language-powerquery-ast
      language-powerquery
      pbix
      m2wasm
    ];
  };

  jupyterEnvironment = jupyter.jupyterlabWith {
    kernels = [ ihaskellWithPackages ];
  };
in
jupyterEnvironment.env
