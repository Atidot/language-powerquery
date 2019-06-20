{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, alex, array, attoparsec, base, happy
      , lens, parsec, stdenv, text
      }:
      mkDerivation {
        pname = "language-powerquery";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson array attoparsec base lens parsec text
        ];
        libraryToolDepends = [ alex happy ];
        executableHaskellDepends = [ base ];
        homepage = "github.com/atidot/language-powerquery";
        description = "PowerQuery Parser";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
