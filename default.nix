{ mkDerivation, base, hpack, stdenv }:
mkDerivation {
  pname = "fixedit";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/kalhauge/fixedit#readme";
  description = "A fixed point conversion";
  license = stdenv.lib.licenses.bsd3;
}
