{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, hspec, lens
      , optparse-applicative, stdenv, text, zip-archive
      }:
      mkDerivation {
        pname = "pbix";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ aeson base lens text zip-archive ];
        executableHaskellDepends = [ base optparse-applicative ];
        testHaskellDepends = [ base bytestring hspec ];
        homepage = "github.com/atidot/pbix";
        description = "PowerBI codec";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
