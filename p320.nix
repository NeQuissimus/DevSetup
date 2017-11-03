{ config, lib, pkgs, ... }:

{
  imports = [ ./nixos-common.nix ./nixos-xmonad.nix ./p320-hardware.nix ];

  boot.initrd.kernelModules = [
    "tun" # VPN
  ];

  boot.loader.systemd-boot.enable = true;

  boot.kernel.sysctl = {
    "vm.max_map_count" = 262144; # Increase map count for ElasticSearch
  };

  networking.hostName = "nixus-desktop";

  networking.hosts = {
    "127.0.0.1" = ["${config.networking.hostName}" "localhost"];
    "10.1.108.103"= ["github.internal" "github.esentire.com"];
    "10.1.110.54" = ["artifactory.internal"];
    "10.1.110.113" = ["gems.internal"];
    "10.1.110.130" = ["build-01.internal"];
    "10.1.110.167" = ["jira.esentire.com" "jira.internal"];
    "10.1.110.191" = ["hipchat.esentire.com"];
    "10.1.110.208" = ["jenkins.internal"];
    "10.1.110.243" = ["gerrit.internal"];
    "10.1.110.57" = ["registry.internal" "portus-1.internal"];
    "10.1.110.83" = ["confluence.internal" "confluence.esentire.com"];
    "10.1.114.20" = ["exchange.esentire.com"];

    "0.0.0.0" = [ "ftp.au.debian.org" ];
  };

  services.kbfs = {
    enable = true;
    mountPoint = "/keybase";
    extraFlags = [
      "-label kbfs"
      "-mount-type normal"
    ];
  };
  services.keybase.enable = true;

  security.pki.certificates = [
    (lib.fileContents ./registry.crt)
  ];

  services.xserver.displayManager.sessionCommands = with pkgs; lib.mkAfter ''
    sleep 3 && ${xlibs.xrandr}/bin/xrandr --output DP1 --crtc 1 --primary --auto --pos 0x0 --output HDMI2 --crtc 2 --rotate left --auto --pos 1920x0
    ${xorg.xsetroot}/bin/xsetroot -solid "#222222" &
    ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &
    ${autocutsel}/bin/autocutsel &
    ${autocutsel}/bin/autocutsel -s PRIMARY &
    ${xlibs.xhost}/bin/xhost + &
  '';

  services.xserver.videoDriver = "intel";

  services.xserver.xrandrHeads = [
    "DP1" { output = "DP1"; primary = true; }
    "HDMI2" { monitorConfig = ''Option "Rotate" "left"''; output = "HDMI2"; }
  ];

  virtualisation.virtualbox.host = {
    enable = false;
    headless = true;
  };
}
