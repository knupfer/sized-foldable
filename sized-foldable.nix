{ mkDerivation, base, ghc-typelits-extra, ghc-typelits-knownnat
, ghc-typelits-natnormalise, stdenv
}:
mkDerivation {
  pname = "sized-foldable";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base ghc-typelits-extra ghc-typelits-knownnat
    ghc-typelits-natnormalise
  ];
  description = "Annotes foldables with their minimum and maximum size without performance impact";
  license = stdenv.lib.licenses.mit;
}
