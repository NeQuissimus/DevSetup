{ ... }:
{
  networking.firewall.logRefusedConnections = false;
  services.journald.extraConfig = "SystemMaxUse=100M";
}
