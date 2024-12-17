# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "ehci_pci" "ahci" "uas" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.supportedFilesystems = [ "ext2" "zfs" ];
  boot.zfs.extraPools = [ "tank" ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/e0f17547-de31-4c04-a8ea-dd3b30efaa3a";
    fsType = "ext4";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/d6e24228-6d2c-47fb-beef-56d2e7f7206e"; }];

  networking.useDHCP = lib.mkDefault true;
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}
