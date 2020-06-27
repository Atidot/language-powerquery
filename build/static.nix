{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc864"
}:
let
  happySrc = fetchGit {
    url = https://github.com/simonmar/happy;
    rev = "27596ff0ce0171d485bf96d38943ffc760923c90";
  };

  cabal2nixOverlay = self: super:
    { buildPackages = super.buildPackages // {
        cabal2nix = super.haskellPackages.cabal2nix;
        happy     = super.haskellPackages.callCabal2nix "happy" "${happySrc}" {};
        #coreutils = super.coreutils.overrideAttrs (oldAttrs: rec {
          #doCheck = false;
        #});
      };
    };

  staticHaskellNixpkgsSrc = fetchTarball https://github.com/nh2/nixpkgs/archive/0c960262d159d3a884dadc3d4e4b131557dad116.tar.gz;
  pkgs = (import staticHaskellNixpkgsSrc { config.allowBroken = true; }).pkgsMusl.appendOverlays [cabal2nixOverlay];

  staticHaskellNixSrc = fetchGit {
    url = https://github.com/nh2/static-haskell-nix;
    rev = "dbce18f4808d27f6a51ce31585078b49c86bd2b5";
  };

  surveyPath = staticHaskellNixSrc + "/survey/default.nix";
  survey = ((import surveyPath) { normalPkgs = pkgs;
                                  approach = "pkgsMusl";
                                  integer-simple = true;
                                }
           );


  haskell = import ./haskell.nix { inherit compiler;
                                           nixpkgs = pkgs;
                                           haskellPackages = survey.haskellPackages;
                                 };
  ease = haskell.ease;
  haskellPackages' = haskell.packages;


  haskellPackages = haskellPackages'.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
      (self: hspkgs: {
        #happy = hspkgs.callCabal2nix "happy" "${happySrc}" {};
      });
  });

  buildStatic = drv: with pkgs.haskell.lib;
    ( disableSharedExecutables
    ( disableSharedLibraries
    ( nixpkgs.lib.flip appendConfigureFlags
        [ "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        ]
    ( drv
    ))));

  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
    (buildStatic language-powerquery-ast)
    (buildStatic language-powerquery)
    (buildStatic pbix)
  ]);

in
pkgs.stdenv.mkDerivation rec {
  name = "language-powerquery-static-env";

  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    haskellEnv
  ];
}
