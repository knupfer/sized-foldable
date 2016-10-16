{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "sized-foldable";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  description = "Annotes foldables with their minimum and maximum size without performance impact";
  license = stdenv.lib.licenses.mit;
}
