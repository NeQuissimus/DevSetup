# https://github.com/NixOS/nixpkgs/pull/242079

{ lib, stdenvNoCC, fetchurl, undmg }:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "alfred-launcher";
  version = "5.6.2_2296";

  src = fetchurl {
    url = "https://cachefly.alfredapp.com/Alfred_${finalAttrs.version}.dmg";
    sha256 = "sha256-bEn7gB7v10T6oSJ2JhxUyuyzFyGNWw0FUyLTPlmAnNc=";
  };
  sourceRoot = ".";

  nativeBuildInputs = [ undmg ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/Applications
    cp -r *.app $out/Applications

    runHook postInstall
  '';
  dontPatchShebangs = true;

  meta = with lib; {
    description = "Application launcher and productivity software";
    homepage = "https://www.alfredapp.com";
    license = licenses.unfree;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    platforms = platforms.darwin;
  };
})
