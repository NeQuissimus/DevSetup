# https://git.catgirl.ai/ext0l/nixos-config/commit/e749b987f2a8b1d68b779b40b024d11a17e8c92a

{ pkgs ? import <nixpkgs> { } }:

with pkgs;

stdenv.mkDerivation rec {
  version = "1.0.5";
  name = "mdloader-${version}";
  src = fetchFromGitHub {
    owner = "Massdrop";
    repo = "mdloader";
    rev = version;
    sha256 = "1pxp9l8p1zb1wlsxd81id4pgcsbljiy863dv85dlxcb2wg2iismy";
  };
  buildInputs = [ makeWrapper ];
  installPhase = ''
    mkdir -p $out/bin
    cp build/mdloader $out/bin
    cp applet-mdflash.bin $out/bin
    wrapProgram $out/bin/mdloader --run "cd $out/bin"
  '';
}
