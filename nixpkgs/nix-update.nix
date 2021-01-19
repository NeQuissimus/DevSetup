{ pkgs ? import <nixpkgs> { } }:

with pkgs;
with pkgs.lib;
with pkgs.python3Packages;

buildPythonApplication rec {
  pname = "nix-update";
  version = "0.3.2+1";

  src = fetchFromGitHub {
    owner = "NeQuissimus";
    repo = pname;
    rev = "acc95546ee1a402d89b444fecacdb1b25aa223fe";
    sha256 = "1wz0abmx0kybxh5ida4vpjrmfjj1rjyam2y10dk9vmrz49780p6z";
  };

  makeWrapperArgs =
    [ "--prefix" "PATH" ":" (lib.makeBinPath [ nixFlakes nix-prefetch ]) ];

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
