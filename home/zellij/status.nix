{ pkgs, ... }:

let
  version = "0.23.0";
in
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  inherit version;

  pname = "zellij-zjstatus";

  src = pkgs.fetchurl {
    url = "https://github.com/dj95/zjstatus/releases/download/v${version}/zjstatus.wasm";
    sha256 = "sha256-4AaQEiNSQjnbYYAh5MxdF/gtxL+uVDKJW6QfA/E4Yf8=";
  };

  phases = [
    "installPhase"
    "patchPhase"
  ];

  installPhase = ''
    mkdir -p "$out/share/zellij/"
    install -Dm0644 $src "$out/share/zellij/zjstatus.wasm"
  '';
})
