{ config, pkgs, lib, ... }:

let 
  interface = "end0";
  ipv4Address = "10.0.0.53";
in {
  imports = [
    ../../nixos/dns.nix
    ../../nixos/nix.nix
    ../../nixos/security.nix 
    ../../nixos/ssh.nix
    ../../nixos/users.nix
    ../../nixos/zsh.nix

    ./hardware.nix
  ];

  console.keyMap = "us";

  documentation.nixos.enable = false;

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    defaultGateway = "10.0.0.2";

    hostName = "rpi4b";

    interfaces."${interface}" = {
      ipv4.addresses = [{
        address = ipv4Address;
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

  services = {
    avahi.enable = true;

    cron = {
      enable = true;
      systemCronJobs = [
        "0 4 * * * root sh -c 'cd /home/nequi/DevSetup && nix flake update && nixos-rebuild boot --flake \".#rpi4b\"'"
        "0 6 * * 0 root reboot"
      ];
    };

    openssh.enable = true;
  };

  specialArgs = { inherit ipv4Address; }

}
