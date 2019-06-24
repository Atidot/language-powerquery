{ mkDerivation, aeson, alex, array, base, happy, hspec
, language-powerquery-ast, lens, monad-loops, optparse-applicative
, stdenv, text, uniplate
}:
mkDerivation {
  pname = "language-powerquery";
  version = "0.1.0.0";
  src = ../language-powerquery;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base language-powerquery-ast lens monad-loops text
    uniplate
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [
    base language-powerquery-ast optparse-applicative
  ];
  testHaskellDepends = [ base hspec language-powerquery-ast ];
  homepage = "github.com/atidot/language-powerquery";
  description = "PowerQuery Parser";
  license = stdenv.lib.licenses.bsd3;
}
