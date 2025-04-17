# https://github.com/NixOS/nixpkgs/pull/242079

{ lib, stdenvNoCC, fetchurl, undmg }:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "alfred-launcher";
  version = "5.6_2290";

  src = fetchurl {
    url = "https://cachefly.alfredapp.com/Alfred_${finalAttrs.version}.dmg";
    sha256 = "sha256-ZF1iM2U4Mf1ME97C7cp+L2kTcOMome330MLLGFl0dBE=";
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
