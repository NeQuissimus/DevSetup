{ config, lib, pkgs, ... }:

with lib;

{
  boot = {
    blacklistedKernelModules = [
        # Obscure network protocols
        "ax25"
        "netrom"
        "rose"
    ];

    cleanTmpDir = true;

    initrd.kernelModules = [
      "ahci"
      "aesni-intel"
      "fuse" # KBFS
      "nls-cp437" # /boot
      "nls-iso8859-1" # /boot
      "vfat" # /boot
    ] ++ optionals (config.virtualisation.docker.enable) [
      "bridge" # Docker
      "br_netfilter" # Docker
      "nf_nat" # Docker
      "veth" # Docker
      "xt_conntrack" # Docker
      "xt_nat" # Docker
    ] ++ optionals (config.virtualisation.docker.enable && !config.virtualisation.docker.liveRestore) [
      "ip_vs" # Docker Swarm
      "vxlan" # Docker Swarm
    ] ++ optionals (config.virtualisation.virtualbox.host.enable) [
      "vboxpci" # VirtualBox
      "vboxnetflt" # VirtualBox
      "vboxnetadp" # VirtualBox
      "vboxdrv" # VirtualBox
    ];

    kernel.sysctl = {
      "kernel.dmesg_restrict" = true; # Restrict dmesg access
      "kernel.kexec_load_disabled" = true; # Prevent kernel reload
      "kernel.kptr_restrict" = lib.mkOverride 500 2; # Hide kernel pointers
      "kernel.unprivileged_bpf_disabled" = true; # Prevent privilege escalation
      "kernel.yama.ptrace_scope" = 1; # Limit ptrace
      "net.core.bpf_jit_enable" = false; # Turn off bpf JIT
      "net.core.bpf_jit_harden" = true; # Harden bpf JIT if it cannot be disabled
      "net.ipv4.conf.all.accept_redirects" = 0; # Don't accept redirects
      "net.ipv4.conf.all.log_martians" = 1; # Log martian packets
      "net.ipv4.conf.all.rp_filter" = 1; # Protect against IP spoofing
      "net.ipv4.conf.all.send_redirects" = 0; # No redirects (only needed on routers)
      "net.ipv4.conf.default.accept_redirects" = 0; # Don't accept redirects
      "net.ipv4.conf.default.log_martians" = 1; # Log martian packets
      "net.ipv4.conf.default.rp_filter" = 1; # Protect against IP spoofing
      "net.ipv4.conf.default.send_redirects" = 0; # No redirects (only needed on routers)
      "net.ipv4.tcp_rfc1337" = 1; # Protect against tcp time-wait assassination hazards
      "net.ipv6.conf.all.accept_redirects" = 0; # Don't accept redirects
      "net.ipv6.conf.default.accept_redirects" = 0; # Don't accept redirects
      "user.max_user_namespaces" = 0; # Disable user namespaces
      "vm.mmap_min_addr" = 65535; # Enforce memory beyond NULL space
      "vm.mmap_rnd_bits" = 32; # Raise ASLR entropy
    };

    kernelPackages = pkgs.linuxPackages_hardened_copperhead;

    kernelParams = [
      "nohibernate" # Disable hibernation
      "page_poison=1" # Poison memory pages, wiping freed memory
      "slab_nomerge" # Disable slab merging (Slab = chunk of memory)
      "slub_debug=FZP" # Enable sanity checks (F), redzoning (Z) and poisoning (P)
      "vsyscall=none" # vsyscall is obsolete
    ];

    loader.systemd-boot.editor = false;
  };

  networking = {
    firewall = {
      allowedTCPPorts = [ ];
      allowPing = false;
      enable = true;
    };

    tcpcrypt.enable = true;
  };

  nix = {
    trustedBinaryCaches = [ http://hydra.nixos.org/ ];
    trustedUsers = [];
  };

  nixpkgs.config.packageOverrides = pkgs: {
    virtualbox = pkgs.virtualbox.override {
      enable32bitGuests = false;
    };
  };

  security = {
    apparmor.enable = true;
    chromiumSuidSandbox.enable = true;
    hideProcessInformation = true;
    lockKernelModules = true;
    sudo.enable = true;
  };

  services = {
    dnscrypt-proxy = {
      enable = true;
      localPort = 43;
    };

    dnsmasq = {
      enable = true;

      servers = [ "127.0.0.1#43" ];
    };
  };

  system.autoUpgrade = {
    channel = mkDefault "https://nixos.org/channels/nixos-17.09";
    dates = "9:00";
    enable = true;
  };

  users.users.root.shell = pkgs.nologin; # Make sure to have a user with a password :D
}
