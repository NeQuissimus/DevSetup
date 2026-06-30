{
  config,
  pkgs,
  lib,
  ipv4Address,
  ...
}:

let
  interface = "end0";
in
{
  imports = [
    (import ../../nixos/dns-cluster.nix { inherit lib ipv4Address; })
    ../../nixos/logs.nix
    ../../nixos/nix.nix
    ../../nixos/security.nix
    ../../nixos/ssh.nix
    ../../nixos/users.nix
    ../../nixos/zsh.nix

    ./hardware.nix
  ];

  console.keyMap = "us";

  documentation.nixos.enable = false;

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    defaultGateway = "10.0.0.2";

    firewall.allowedTCPPorts = [ 9002 ];

    hostName = "rpi4b";

    interfaces."${interface}" = {
      ipv4.addresses = [
        {
          address = ipv4Address;
          prefixLength = 16;
        }
      ];
      ipv6.addresses = [
        {
          address = "fd00:1873::200";
          prefixLength = 117;
        }
      ];
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
        "0 3 30 * 0 nequi bash -c 'cd /home/nequi/DevSetup && nix flake update'"
        "0 3 30 * 0 root echo -e \"[safe]\n\tdirectory = /home/nequi/DevSetup\" > /root/.gitconfig"
        "0 4 * * 0 root bash -c 'cd /home/nequi/DevSetup && nixos-rebuild boot --flake \".#rpi4b\"'"
        "0 6 * * 0 root reboot"
      ];
    };

    journald.extraConfig = "SystemMaxUse=100M";

    openssh.enable = true;

    prometheus.exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
        port = 9002;
      };
    };
  };
}
