{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    xwayland-satellite # xwayland support
  ];
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  programs.niri.enable = true;

  services = {
    displayManager.sddm.enable = false;

    greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.tuigreet}/bin/tuigreet --time --cmd ${pkgs.niri}/bin/niri --user-menu";
          user = "nequi";
        };
      };
    };
  };

  services.gnome.gcr-ssh-agent.enable = false;

  systemd.user.services.niri.enableDefaultPath = false;
}
