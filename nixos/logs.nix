{ ... }:
{
  networking.firewall.logRefusedConnections = false;
  services.journald.settings.Journal.SystemMaxUse = "100M";
}
