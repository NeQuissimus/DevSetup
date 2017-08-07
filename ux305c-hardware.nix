# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/22ebf61e-926e-4038-bc53-1d6832f1fb51";
      fsType = "ext4";
      options = [ "noatime" "nodiratime" "commit=300" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/86AB-210F";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/1d5e662b-89e5-4aac-88af-7f9207898a90"; }
    ];

  nix.maxJobs = lib.mkDefault 4;
}
