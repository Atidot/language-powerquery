{ mkDerivation, aeson, base, hspec, lens, stdenv, text }:
mkDerivation {
  pname = "language-powerquery-ast";
  version = "0.1.0.0";
  src = ../language-powerquery-ast;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base lens text ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  homepage = "github.com/atidot/language-powerquery";
  description = "PowerQuery AST Types";
  license = stdenv.lib.licenses.bsd3;
}
