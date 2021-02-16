{ pkgs ? import <nixpkgs> { } }:

with pkgs;

python3Packages.buildPythonPackage {
  name = "kconfig-hardened-check";
  src = pkgs.fetchFromGitHub {
    rev = "2f8e7a4dc57a0e0e192dd303bd89f9be0e9240a4";
    repo = "kconfig-hardened-check";
    owner = "a13xp0p0v";
    sha256 = "sha256-LJDL1OaZUnDusS6D48n6lHlgALh3Ig/4aVjMN0wegm8=";
  };
}
