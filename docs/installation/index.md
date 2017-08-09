# Installation from scratch

## Prerequisites

- A computer we don't mind messing with
- [NixOS 17.09](https://nixos.org/nixos/download.html) minimal installation CD, on a USB stick

## Assumptions

This installation will go beyond the excellent [NixOS Manual](https://nixos.org/nixos/manual/) and explain a simple installation of NixOS on a [LUKS](https://en.wikipedia.org/wiki/Linux_Unified_Key_Setup)-encrypted hard drive.
I am using an [ASUS Zenbook UX305CA](https://www.asus.com/us/Laptops/ASUS-ZenBook-UX305CA/), which is why neither an optical drive for the installation medium nor an Ethernet connection for package retrieval are an option.

Please refer to the NixOS manual ([Installing NixOS](https://nixos.org/nixos/manual/index.html#sec-installation)) for a simpler version of this guide, in case you do not need LUKS or WiFi.
Or mix and match instructions as you see fit, this guide stays very close to the manual.

Note that the basic LUKS setup is very inspired by a [Martijn Vermaat Gist](https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134).

## Goal

What do we want out of this? A computer with a working NixOS installation, of course!

When do we want it? Within about **10 minutes**!

## Drive partitioning

First, we need to make some space on our hard drive.
The easiest setup to go with is a small partition for all data related to booting the machine and a large partition to house the rest of the data.

- /boot - 2 GiB - type ef00 (EFI)
- / - 246 GiB - type 8300 (Linux)

```bash
$ gdisk /dev/sda
```

In the interface, we need to do the following:

- `o` to start with a new partition table
- `n` to add a new partition, leave the start sector (hit ENTER) and define the last sector as +2G (a 2 GiB partition) with type `ef00` (EFI)
- `n` again, leave start and end sectors (this will assign the remaining space), type `8300` (Linux)
- `w` to write the changes to disk (**This will erase your old partition table on that disk and make it hard(er) to recover data; consider your old data lost at this point**)

We are sent back to our Linux shell with a hard drive that is ready to go. At this point, we could just format the partitions and install NixOS. But we said we wanted to use LUKS to encrypt our data. This is when that happens!

## LUKS setup

Assuming you followed the above instructions, an `ls /dev/sd*` will reveal that the drive `/dev/sda` we partitioned earlier is now split into two: `/dev/sda1` and `/dev/sda2`. Because we created the smaller partition first, it became `/dev/sda1`.

We need LUKS to encrypt the larger partition, where most of our system and user data will be held.

```bash
$ cryptsetup luksFormat /dev/sda2
$ cryptsetup luksOpen /dev/sda2 enc-pv
```

OK, what happened?

First, we formatted `/dev/sda2` to use LUKS (and implicitly followed the instructions; right?). A variety of things happened in the background (you may have noticed what seemed like a "pause"), but whatever that was, our partition is ready for us! Feel free to search the web for what `cryptsetup luksFormat` really does!
Next, we "open" the partition and make it available in a mapper with the name `env-pv`.

Now it is time to make the encrypted partition usable for our system installation!

## LVM setup

```bash
$ pvcreate /dev/mapper/enc-pv
$ vgcreate vg /dev/mapper/enc-pv
$ lvcreate -L 6G -n swap vg
$ lvcreate -l '100%FREE' -n root vg
```

The above commands use [LVM](https://en.wikipedia.org/wiki/Logical_Volume_Manager_(Linux)) to create logical volumes - "partitions" inside our encrypted partition.
We ask for a volume group by the name of `vg` (**V**olume **G**roup; so smart, right?). <!-- *** Fix Atom syntax highlighter -->
We create two logical volumes, one named "swap" (6 GiB in size), the other named "root" (using all the remaining space).

Almost ready to go, the hard part is done!

## Formatting

```bash
$ mkfs.fat /dev/sda1
$ mkfs.ext4 -L root /dev/vg/root
$ mkswap -L swap /dev/vg/swap

$ mount /dev/vg/root /mnt
$ mkdir /mnt/boot
$ mount /dev/sda1 /mnt/boot
$ swapon /dev/vg/swap
```

We format our 2 GiB `/dev/sda1` to use the FAT file system, our "root" volume uses ext4 and we turn on the swap.
The volumes get mounted into `/mnt` and `/mnt/boot`, respectively.

Let's get this show on the road! But wait! We are using the minimal CD, let's fire up our WiFi, so we can pull packages from an online source.

## WiFi

```bash
$ wpa_passphrase 'SSID' 'PASSWORD' > /etc/wpa_supplicant.conf
$ systemctl start wpa_supplicant
$ nix-channel --update
```

Replace `SSID` and `PASSWORD` with your own SSID and PSK for your wireless access point ("router") at home/work/coffee shop.

`nix-channel --update` pulls the latest package index from the NixOS binary cache (`nix-channel --list` shows you where exactly)

OK, the grunt work has been completed. Let's Nix it up!

## Configuration

```bash
$ nixos-generate-config --root /mnt
$ nano /mnt/etc/nixos/configuration.nix
```

The above command will generate a sample configuration for our machine and place it into `/mnt/etc/nixos`.
We will want to edit the configuration to make sure our LUKS setup is recognized.
Change it to be something similar to the following, which is the minimum we need right now:

```nix
{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [{
    name = "root";
    device = "/dev/disk/by-uuid/SOME-UUID";
    preLVM = true;
    allowDiscards = true;
  }];

  networking.wireless = {
    enable = true;
    networks.SSID.pskRaw = "ee5df011c243ddd259479a67fa6a512410e048f61a4635335fb1s36bc5511f08";
    networks.SSID2.psk = "helloworld";
  };
}
```

Make sure to replace `SOME-UUID` with your drive's UUID (using `blkid /dev/sda2`).
`wpa_passphrase` will allow you to generate the encrypted version of your WiFi password. Or just use a plain PSK (I don't have to tell you how insecure this is, right?).

## Installation

Here comes the hardest part... Just kidding, this is where NixOS will blow you away:

```bash
$ nixos-install
$ reboot
```

Yes, that is it! I promise! A couple of prompts (you will want to set a `root` password) and your system will be ready to go.
Pull the USB stick out and watch NixOS boot up on the computer for the first time.

Head over to the [System configuration - Basics](../config-basic) for the next steps. After all, we have a very plain Linux system that boots up but does pretty much nothing else.
