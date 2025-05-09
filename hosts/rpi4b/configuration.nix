{ config, pkgs, lib, ... }:

let interface = "end0";
in {
  imports = [
    ./hardware.nix

    ../../nixos/dns.nix
    ../../nixos/nix.nix
    ../../nixos/security.nix
    ../../nixos/ssh.nix
    ../../nixos/users.nix
    ../../nixos/zsh.nix
  ];

  console.keyMap = "us";

  documentation.nixos.enable = false;

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    defaultGateway = "10.0.0.2";

    hostName = "rpi4b";

    interfaces."${interface}" = {
      ipv4.addresses = [{
        address = "10.0.0.53";
        prefixLength = 16;
      }];
      ipv6.addresses = [{
        address = "fd00:1873::200";
        prefixLength = 117;
      }];
    };

    nameservers = [ "9.9.9.9" ];
    useDHCP = false;
    usePredictableInterfaceNames = true;
  };

  nixpkgs.system = "aarch64-linux";

  services = {
    avahi.enable = true;

    cron = {
      enable = true;
      systemCronJobs = [ "0 6 * * 0 root reboot" ];
    };

    openssh.enable = true;

    syslogd = {
      defaultConfig = "*.* @10.0.0.52:5514";

      enable = true;
    };
  };

  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-unstable";
}
