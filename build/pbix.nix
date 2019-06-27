{ mkDerivation, aeson, base, bytestring, hspec, language-powerquery
, language-powerquery-ast, lens, optparse-applicative, stdenv, text
, zip-archive
}:
mkDerivation {
  pname = "pbix";
  version = "0.1.0.0";
  src = ../pbix;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring language-powerquery language-powerquery-ast
    lens text zip-archive
  ];
  executableHaskellDepends = [ base optparse-applicative ];
  testHaskellDepends = [ base hspec ];
  homepage = "github.com/atidot/pbix";
  description = "PowerBI codec";
  license = stdenv.lib.licenses.bsd3;
}
