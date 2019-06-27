{ mkDerivation, aeson, base, bytestring, hspec, language-powerquery
, language-powerquery-ast, lens, optparse-applicative, stdenv, text
, wasm
}:
mkDerivation {
  pname = "m2wasm";
  version = "0.1.0.0";
  src = ../m2wasm;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring language-powerquery language-powerquery-ast
    lens text wasm
  ];
  executableHaskellDepends = [ base optparse-applicative ];
  testHaskellDepends = [ base hspec ];
  homepage = "github.com/atidot/language-powerquery";
  description = "PowerQuery -> WASM compilation";
  license = stdenv.lib.licenses.bsd3;
}
