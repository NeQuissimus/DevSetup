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
    owner = "soerenschneider";
    repo = pname;
    rev = "3dc0ca073d97af50f0085f82c89513063b2efd77";
    sha256 = "sha256-FQrPBSgLS4oL4c7mT9roWHOSIpXeIOtw5getJ1N4pf8=";
  };
}
