{ config, lib, pkgs, ... }: {
  programs.firejail = {
    enable = true;
    wrappedBinaries = {
      firefox = {
        executable = "${lib.getBin pkgs.firefox}/bin/firefox";
        extraArgs = [ "--private=~/work" ];
        profile = "${pkgs.firejail}/etc/firejail/firefox.profile";
      };
    };
  };
}
