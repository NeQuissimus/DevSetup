{ pkgs ? <nixpkgs>, lib }:

with pkgs;
with pkgs.python3Packages;

buildPythonApplication rec {
  pname = "nix-update";
  version = "0.2+1";

  src = fetchFromGitHub {
    owner = "NeQuissimus";
    repo = pname;
    rev = "a0579974e65c648cfa491b8b3b519b773fea1ae1";
    sha256 = "sha256-MGNN6XHUMsqHtfS862EFh75QR1YuF3+MozMns9Kttxw=";
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
