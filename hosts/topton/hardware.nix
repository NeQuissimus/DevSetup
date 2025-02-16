{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    extraModulePackages = [ ];

    initrd = {
      availableKernelModules =
        [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
      kernelModules = [ ];
      postDeviceCommands = lib.mkAfter ''
        zfs rollback -r system-pool/cache@empty
      '';
    };

    kernelModules = [ "kvm-intel" ];
    zfs.extraPools = [ "tank" ];
  };

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=2G" "mode=755" ];
  };

  fileSystems."/nix" = {
    device = "system-pool/nix";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "system-pool/home";
    fsType = "zfs";
  };

  fileSystems."/var/cache" = {
    device = "system-pool/cache";
    fsType = "zfs";
  };

  fileSystems."/etc/gcs" = {
    device = "/nix/persist/etc/gcs";
    fsType = "none";
    options = [ "bind" ];
  };

  fileSystems."/etc/nixos" = {
    device = "/nix/persist/etc/nixos";
    fsType = "none";
    options = [ "bind" ];
  };

  fileSystems."/var/log" = {
    device = "/nix/persist/var/log";
    fsType = "none";
    options = [ "bind" ];
  };

  fileSystems."/var/lib" = {
    device = "/nix/persist/var/lib";
    fsType = "none";
    options = [ "bind" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/BB50-11D5";
    fsType = "vfat";
    options = [ "fmask=0022" "dmask=0022" ];
  };

  swapDevices = [ ];

  networking.interfaces.enp4s0.useDHCP = true;

  nixpkgs.hostPlatform = "x86_64-linux";
}
