{ pkgs ? import <nixpkgs> { } }:

with pkgs;
with pkgs.lib;
with pkgs.python3Packages;

buildPythonApplication rec {
  pname = "dhcpd-leases-exporter";
  version = "1.0.1";
  format = "other";

  propagatedBuildInputs = with python3Packages; [ prometheus-client ];

  buildPhase = ''

  '';

  installPhase = ''
    cp -dr --no-preserve='ownership' . $out/
    sed -i '1 i\#! ${pkgs.python3}/bin/python' $out/dhcpd_lease_exporter.py
    chmod +x $out/dhcpd_lease_exporter.py
    wrapProgram $out/dhcpd_lease_exporter.py \
      --prefix PYTHONPATH : "$PYTHONPATH:${python3Packages.prometheus-client}:$out:"
  '';

  src = fetchFromGitHub {
    owner = "NeQuissimus";
    repo = pname;
    rev = "12d839a5fc444f81d4a9b5e424680521258a2fd3";
    sha256 = "sha256-ycPpcbUaWNJ5xVy1XriQFYeYLiYAplN+71ufwMeox28=";
  };
}
