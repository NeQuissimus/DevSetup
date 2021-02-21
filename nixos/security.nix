{ config, lib, pkgs, ... }:

{
  security = {
    allowSimultaneousMultithreading = true;
    apparmor.enable = true;
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };
}
