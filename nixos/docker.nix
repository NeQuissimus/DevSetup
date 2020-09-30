{ config, lib, pkgs, ... }:

{
  environment.etc."docker-daemon.json".text = ''{"bip": "192.168.1.5/24"}'';

  virtualisation.docker = {
    enable = lib.mkDefault false;
    extraOptions = "--config-file=/etc/docker-daemon.json";
  };
}
