{ config, pkgs, lib, ipv4Address, ... }:

let 
  interface = "enP4p65s0";
in {
  imports = [
    ../../nixos/dns.nix
    ../../nixos/nix.nix
    ../../nixos/observability.nix
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

    hostName = "opi5plus";

    interfaces."${interface}" = {
      ipv4.addresses = [{
        address = ipv4Address;
        prefixLength = 16;
      }];
      ipv6.addresses = [{
        address = "fd00:1873::300";
        prefixLength = 117;
      }];
    };

    nameservers = [ "9.9.9.9" ];
    useDHCP = false;
    usePredictableInterfaceNames = true;
  };

  nixpkgs.hostPlatform = "aarch64-linux";

  services = {
    avahi.enable = true;

    cron = {
      enable = true;
      systemCronJobs = [
        "0 3 30 * 0 nequi bash -c 'cd /home/nequi/DevSetup && git checkout -- . && git pull && nix flake update'"
        "0 3 30 * 0 root echo -e \"[safe]\n\tdirectory = /home/nequi/DevSetup\" > /root/.gitconfig"
        "0 4 * * 0 root bash -c 'cd /home/nequi/DevSetup && nixos-rebuild boot --flake \".#opi5plus\" && reboot'"
      ];
    };

    openssh.enable = true;
  };
}
