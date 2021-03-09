{ pkgs, ... }:
{
  nixpkgs.config = {
    packageOverrides = pkgs: {
       openssl = pkgs.libressl.override {
         fetchurl = pkgs.fetchurlBoot;
       };
    };
  };
}
