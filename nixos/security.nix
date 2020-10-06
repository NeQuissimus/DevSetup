{ config, lib, pkgs, ... }:

{
  security = {
    allowSimultaneousMultithreading = true;
    apparmor.enable = true;
    rngd.enable = false;
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };
}
