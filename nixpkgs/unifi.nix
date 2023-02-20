{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let 
  version = "7.3.83";
  sha256 = "sha256-mDbF9vfoWjvABxcOIRngStji7xBllgZfUhVA9kF6ERU=";
in stdenv.mkDerivation ({
    inherit version;
    pname = "unifi-controller";

    src = fetchurl {
      url = "https://dl.ubnt.com/unifi/${version}/unifi_sysvinit_all.deb";
      inherit sha256;
    };

    nativeBuildInputs = [ dpkg ];

    unpackPhase = ''
      runHook preUnpack
      dpkg-deb -x $src ./
      runHook postUnpack
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cd ./usr/lib/unifi
      cp -ar dl lib webapps $out
      runHook postInstall
    '';

    passthru.tests = {
      unifi = nixosTests.unifi;
    };

    meta = with lib; {
      homepage = "http://www.ubnt.com/";
      description = "Controller for Ubiquiti UniFi access points";
      sourceProvenance = with sourceTypes; [ binaryBytecode ];
      license = licenses.unfree;
      platforms = platforms.unix;
      maintainers = with maintainers; [ erictapen globin patryk27 pennae ];
    };
  })