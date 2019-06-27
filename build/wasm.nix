{ mkDerivation, alex, array, base, base64-bytestring, bytestring
, cereal, containers, deepseq, directory, fetchgit, filepath, happy
, ieee754, mtl, optparse-applicative, stdenv, tasty, tasty-hunit
, text, transformers, utf8-string, vector
}:
mkDerivation {
  pname = "wasm";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/SPY/haskell-wasm";
    sha256 = "0hwvppsyhpx2j1pqnkpigjycd1xw7brm4wh3hnh4ib4yd5kjwbks";
    rev = "8b29d3f51bcab2acb39982931486a558c6162985";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bytestring cereal containers deepseq ieee754 mtl text
    transformers utf8-string vector
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [
    base base64-bytestring bytestring optparse-applicative
  ];
  testHaskellDepends = [
    base bytestring directory filepath mtl tasty tasty-hunit text
  ];
  testToolDepends = [ alex happy ];
  jailbreak = true;
  doCheck = false;
  homepage = "https://github.com/SPY/haskell-wasm/";
  description = "WebAssembly Language Toolkit and Interpreter";
  license = stdenv.lib.licenses.mit;
}
