{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
}:
with nixpkgs;
let
  reflexPlatformSrc = fetchGit {
    url = https://github.com/reflex-frp/reflex-platform;
    rev = "716879f16d53c93766e7ed9af17416fccb2edfe1";
  };

  reflexWidgetsSrc = fetchGit {
    url = https://github.com/atidot/reflex-widgets;
    rev = "d270d3c63e1d3d3399343cd2792734b30a9b96cb";
  };

  reflex-platform = import reflexPlatformSrc {};
in
reflex-platform.project({ pkgs, ... }: {
  packages = {
    language-powerquery-ast    = ../language-powerquery-ast;
    language-powerquery        = ../language-powerquery;
    language-powerquery-editor = ../language-powerquery-editor;

    reflex-utils      = reflexWidgetsSrc + "/reflex-utils";
    reflex-mdl        = reflexWidgetsSrc + "/reflex-mdl";
    reflex-chartjs    = reflexWidgetsSrc + "/reflex-chartjs";
    reflex-jsoneditor = reflexWidgetsSrc + "/reflex-jsoneditor";
    reflex-codemirror = reflexWidgetsSrc + "/reflex-codemirror";
    reflex-select2    = reflexWidgetsSrc + "/reflex-select2";
  };

  shells = {
    ghcjs = [ "language-powerquery-editor"
            ];
  };
})

