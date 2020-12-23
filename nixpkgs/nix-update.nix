{ pkgs ? import <nixpkgs> {} }:

with pkgs;
with pkgs.lib;
with pkgs.python3Packages;

buildPythonApplication rec {
  pname = "nix-update";
  version = "0.2+4";

  src = fetchFromGitHub {
    owner = "NeQuissimus";
    repo = pname;
    rev = "4523d1027e828a4d4da065aaadc7c271a55379b7";
    sha256 = "0610l7fih0lwm1h6b3xb3c2qxy3xjr4g7hl5bb1c6lscix4xcc2j";
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
