{ config, lib, pkgs, ... }:

{
  imports = [ ./nixos-common.nix ./t460s-hardware.nix ./xmonad-config.nix ./t460s-wifi.nix ];

  boot.loader.systemd-boot.enable = true;

  networking.hostName = "nixus";

  networking.hosts = {
    "127.0.0.1" = ["${config.networking.hostName}" "localhost"];
    "0.0.0.0" = ["ftp.au.debian.org"];
    "10.1.108.103"= ["github.internal" "github.esentire.com"];
    "10.1.110.113" = ["gems.internal"];
    "10.1.110.130" = ["build-01.internal"];
    "10.1.110.167" = ["jira.esentire.com" "jira.internal"];
    "10.1.110.191" = ["hipchat.esentire.com"];
    "10.1.110.208" = ["jenkins.internal"];
    "10.1.110.243" = ["gerrit.internal"];
    "10.1.110.57" = ["registry.internal" "portus-1.internal"];
    "10.1.110.83" = ["confluence.internal" "confluence.esentire.com"];
    "10.1.114.20" = ["exchange.esentire.com"];
  };

  security.pki.certificates = [
    (lib.fileContents ./registry.crt)
  ];

  security.sudo.wheelNeedsPassword = false;

  services.xserver.displayManager.sessionCommands = with pkgs; lib.mkAfter ''
    ${xlibs.xrandr}/bin/xrandr --output DP2-3 --crtc 1 --auto --pos 0x0 --output DP2-2 --crtc 2 --primary --auto --pos 1920x0 --output eDP1 --auto --pos 3840x0 &
    ${xorg.xsetroot}/bin/xsetroot -solid "#222222" &
    ${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr &
    ${autocutsel}/bin/autocutsel &
    ${autocutsel}/bin/autocutsel -s PRIMARY &
    ${xlibs.xhost}/bin/xhost + &
  '';

  services.xserver.resolutions = [
    { x = 1920; y = 1080; }
    { x = 1280; y = 800; }
    { x = 1024; y = 768; }
  ];

  services.xserver.videoDriver = "intel";

  services.xserver.xrandrHeads = [ "DP2-3" "DP2-2" "eDP1" ];

  users.extraUsers.kubernetes = {
    extraGroups = [ "docker" ];
    isNormalUser = true;
    name = "kubernetes";
    uid = 1002;
  };

  users.extraUsers.nix = {
    extraGroups = [ "docker" ];
    isNormalUser = true;
    name = "nix";
    uid = 1003;
  };

  users.extraUsers.ruby = {
    extraGroups = [ "docker" ];
    isNormalUser = true;
    name = "ruby";
    uid = 1004;
  };

  users.extraUsers.scala = {
    extraGroups = [ "docker" ];
    isNormalUser = true;
    name = "scala";
    uid = 1001;
  };

  virtualisation.virtualbox.host.enable = false;
}
