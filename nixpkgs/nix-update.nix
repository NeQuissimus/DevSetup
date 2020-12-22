{ pkgs ? import <nixpkgs> {} }:

with pkgs;
with pkgs.lib;
with pkgs.python3Packages;

buildPythonApplication rec {
  pname = "nix-update";
  version = "0.2+3";

  src = fetchFromGitHub {
    owner = "NeQuissimus";
    repo = pname;
    rev = "73b2fad6ca2be8ec4b160640db4d1c2ce609d293";
    sha256 = "059nl23ma1bsg43a0qb8nsf3gaw3psxswycb1y0f99gc3ph3czwz";
  };

  makeWrapperArgs = [
    "--prefix" "PATH" ":" (lib.makeBinPath [ nixFlakes nix-prefetch ])
  ];

  checkPhase = ''
    $out/bin/nix-update --help >/dev/null
  '';

  meta = with lib; {
    description = "Swiss-knife for updating nix packages";
    inherit (src.meta) homepage;
    license = licenses.mit;
    maintainers = with maintainers; [ mic92 zowoq ];
    platforms = platforms.all;
  };
}
