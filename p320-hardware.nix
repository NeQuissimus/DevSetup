# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" "sr_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/ca968e2b-f027-4c25-ab9a-12c8d2b04d6a";
      fsType = "btrfs";
      options = [ "noatime" "compress=lzo" "ssd" "discard" "space_cache" "commit=120" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/2135-8872";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/3e1f7c72-bb92-4c83-a4cb-934c10b4b5d5"; }
    ];

  nix.maxJobs = lib.mkDefault 8;
}