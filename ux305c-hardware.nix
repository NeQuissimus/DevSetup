{ config, lib, pkgs, ... }:

{
  imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/nixos@blank
  '';

  fileSystems."/" = {
    device = "rpool/nixos";
    fsType = "zfs";
    options = [ ];
  };

  fileSystems."/home" = {
    device = "rpool/home";
    fsType = "zfs";
    options = [ "nosuid" ];
  };

  fileSystems."/nix" = {
    device = "rpool/nix";
    fsType = "zfs";
    options = [ "nosuid" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/3705-CC99";
    fsType = "vfat";
    options = [ "nosuid" "noexec" ];
  };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 2;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
