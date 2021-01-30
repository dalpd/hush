{ mkDerivation, base, ghcid, nixfmt, stdenv, zlib }:
mkDerivation {
  pname = "scully";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ghcid nixfmt zlib ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
  shellHook = ''
      hpack
    '';
}
