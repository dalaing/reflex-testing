{ mkDerivation, base, criterion, ghcjs-dom, hedgehog, jsaddle
, jsaddle-dom, mmorph, mtl, reflex, reflex-dom, reflex-dom-core
, stdenv, stm, text, transformers
}:
mkDerivation {
  pname = "reflex-testing";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base criterion ghcjs-dom hedgehog jsaddle jsaddle-dom mmorph mtl
    reflex reflex-dom reflex-dom-core stm text transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
