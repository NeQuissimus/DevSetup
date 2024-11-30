# https://github.com/NixOS/nixpkgs/pull/242079

{ lib, stdenvNoCC, fetchurl, undmg }:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "alfred-launcher";
  version = "5.5.1_2273";

  src = fetchurl {
    url = "https://cachefly.alfredapp.com/Alfred_${finalAttrs.version}.dmg";
    sha256 = "sha256-BopF9IV/JOpu/aViwV4nDxivlQUZmN+K3+f1/7BaN6M=";
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
