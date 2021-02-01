{ config, lib, pkgs, ... }:

{
  networking = {
    enableIPv6 = true;

    firewall = {
      allowedTCPPorts = lib.mkDefault [ ];
      allowedUDPPorts = lib.mkDefault [ ];
      allowPing = false;
      enable = true;
    };
  };

  networking.timeServers = [ "10.0.10.6" ];

  services = {
    dnsmasq = {
      alwaysKeepRunning = true;
      enable = lib.mkDefault true;

      extraConfig = ''
        no-hosts
        no-negcache
        all-servers
        cache-size=2000
        local-ttl=3600
        min-cache-ttl=3600
      '';

      resolveLocalQueries = false;
    };

    nscd.enable = lib.mkDefault true;
  };
}
