{ pkgs ? import <nixpkgs> {} }:

let
  vanillaKernel = pkgs.linux_4_9;
  grsecPatch = rec {
    name = "grsec";
    patch = pkgs.fetchpatch {
      name = "grsec.patch";
      url = https://github.com/minipli/linux-unofficial_grsec/releases/download/v4.9.74-unofficial_grsec/v4.9.74-unofficial_grsec-20180103130648.diff;
      sha256 = "0fyi3pbiz1lxnjshz5ra3zzv02nxz9jj9dag465dn4z44w3g5jlg";
    };
  };

  grsecKernel = (vanillaKernel.override {
    kernelPatches = vanillaKernel.kernelPatches ++ [
      grsecPatch
      pkgs.kernelPatches.tag_hardened
    ];
    modDirVersionArg = vanillaKernel.modDirVersion + "-hardened";
  });
in
  pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor grsecKernel)
