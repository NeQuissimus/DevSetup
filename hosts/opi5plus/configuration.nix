{
  config,
  pkgs,
  lib,
  ipv4Address,
  ...
}:

let
  interface = "enP4p65s0";
in
{
  imports = [
    (import ../../nixos/dns-cluster.nix { inherit lib ipv4Address; })
    ../../nixos/logs.nix
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

  environment.systemPackages = with pkgs; [
    git
    htop
  ];

  i18n = {
    defaultLocale = "en_CA.UTF-8";
  };

  networking = {
    defaultGateway = "10.0.0.2";

    hostName = "opi5plus";

    interfaces."${interface}" = {
      ipv4.addresses = [
        {
          address = ipv4Address;
          prefixLength = 16;
        }
      ];
      ipv6.addresses = [
        {
          address = "fd00:1873::300";
          prefixLength = 117;
        }
      ];
    };

    nameservers = [ "9.9.9.9" ];
    useDHCP = false;
    usePredictableInterfaceNames = true;
  };

  nixpkgs.hostPlatform = "aarch64-linux";

  services = {
    avahi.enable = true;

    openssh.enable = true;
  };
}
