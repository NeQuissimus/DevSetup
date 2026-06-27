{
  config,
  lib,
  pkgs,
  ...
}:

{
  system.autoUpgrade = {
    allowReboot = true;
    dates = "daily";
    enable = true;
    flake = lib.mkDefault "github:NeQuissimus/DevSetup#${config.networking.hostName}";
    operation = "boot";
  };

  security = {
    allowSimultaneousMultithreading = true;
    apparmor.enable = true;
    sudo = {
      enable = true;
      execWheelOnly = true;
      wheelNeedsPassword = true;
    };
  };

  services.fail2ban = {
    bantime = "24h"; # Ban IPs for one day on the first ban

    bantime-increment = {
      enable = true;
      formula = "ban.Time * math.exp(float(ban.Count+1)*banFactor)/math.exp(1*banFactor)";
      maxtime = "168h";
      overalljails = true;
    };

    enable = true;
  };
}
