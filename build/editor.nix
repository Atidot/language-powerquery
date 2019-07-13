{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
}:
with nixpkgs;
let
  reflexPlatformSrc = fetchGit {
    url = https://github.com/reflex-frp/reflex-platform;
    rev = "716879f16d53c93766e7ed9af17416fccb2edfe1";
  };

  reflex-platform = import reflexPlatformSrc {};
in
reflex-platform.project({ pkgs, ... }: {
  packages = {
    language-powerquery-ast    = ../language-powerquery-ast;
    language-powerquery        = ../language-powerquery;
    language-powerquery-editor = ../language-powerquery-editor;
  };

  shells = {
    ghcjs = [ "language-powerquery-ast"
              "language-powerquery"
              "language-powerquery-editor"
            ];
  };
})

