{ config, lib, pkgs, ... }:

{
  boot = {
    blacklistedKernelModules = [
      # Obscure network protocols
      "ax25"
      "netrom"
      "rose"

      # Old or rare or insufficiently audited filesystems
      "adfs"
      "affs"
      "bfs"
      "befs"
      "cramfs"
      "efs"
      "erofs"
      "exofs"
      "freevxfs"
      "f2fs"
      "hfs"
      "hpfs"
      "jfs"
      "minix"
      "nilfs2"
      "ntfs"
      "omfs"
      "qnx4"
      "qnx6"
      "sysv"
      "ufs"
    ];

    initrd.kernelModules = lib.unique ([
      "ahci"
      "aesni-intel"
      "iso9660" # Mount CDs
      "nls-cp437" # /boot
      "nls-iso8859-1" # /boot
      "tun"
      "usbhid"
      "vfat" # /boot
    ] ++ lib.optionals (config.services.kbfs.enable) [
      "fuse" # KBFS
    ] ++ lib.optionals
      (lib.any (x: x == "exfat") config.boot.supportedFilesystems) [
        "fuse" # exfat
      ] ++ lib.optionals (config.virtualisation.docker.enable) [
        "bridge" # Docker
        "br_netfilter" # Docker
        "nf_nat" # Docker
        "veth" # Docker
        "xt_conntrack" # Docker
        "xt_nat" # Docker
      ] ++ lib.optionals (config.virtualisation.docker.enable
        && !config.virtualisation.docker.liveRestore) [
          "ip_vs" # Docker Swarm
          "vxlan" # Docker Swarm
        ] ++ lib.optionals (config.virtualisation.virtualbox.host.enable) [
          "vboxpci" # VirtualBox
          "vboxnetflt" # VirtualBox
          "vboxnetadp" # VirtualBox
          "vboxdrv" # VirtualBox
        ]);

    kernel.sysctl = {
      "vm.dirty_background_ratio" =
        20; # Max % of RAM with dirty pages before reclaim
      "vm.dirty_ratio" = 30; # Max % of RAM with dirty pages before STW
      "vm.dirty_writeback_centisecs" = 500; # Frequency pages are reclaimed
      "vm.dirty_expire_centisecs" = 3000; # Max age of dirty pages
      "vm.drop_caches" = 1; # Drop caches early
      "vm.swappiness" = 1; # Minimum swap usage
      "vm.vfs_cache_pressure" = 60; # Less reclaim pressure

      "kernel.dmesg_restrict" = true; # Restrict dmesg access
      "kernel.kexec_load_disabled" = true; # Prevent kernel reload
      "kernel.kptr_restrict" = lib.mkOverride 500 2; # Hide kernel pointers
      "kernel.unprivileged_bpf_disabled" = true; # Prevent privilege escalation
      "kernel.yama.ptrace_scope" = 1; # Limit ptrace
      "net.core.bpf_jit_enable" = false; # Turn off bpf JIT
      "net.core.bpf_jit_harden" =
        true; # Harden bpf JIT if it cannot be disabled
      "net.ipv4.conf.all.accept_redirects" = 0; # Don't accept redirects
      "net.ipv4.conf.all.log_martians" = 1; # Log martian packets
      "net.ipv4.conf.all.rp_filter" = 1; # Protect against IP spoofing
      "net.ipv4.conf.all.send_redirects" =
        0; # No redirects (only needed on routers)
      "net.ipv4.conf.default.accept_redirects" = 0; # Don't accept redirects
      "net.ipv4.conf.default.log_martians" = 1; # Log martian packets
      "net.ipv4.conf.default.rp_filter" = 1; # Protect against IP spoofing
      "net.ipv4.conf.default.send_redirects" =
        0; # No redirects (only needed on routers)
      "net.ipv4.tcp_rfc1337" =
        1; # Protect against tcp time-wait assassination hazards
      "net.ipv6.conf.all.accept_redirects" = 0; # Don't accept redirects
      "net.ipv6.conf.default.accept_redirects" = 0; # Don't accept redirects
      #      "user.max_user_namespaces" = 0; # Disable user namespaces, breaks Nix 2.0
      "vm.mmap_min_addr" = 65535; # Enforce memory beyond NULL space
      "vm.mmap_rnd_bits" = 32; # Raise ASLR entropy
    };

    kernelPackages = lib.mkDefault pkgs.linuxPackages_latest_hardened;

    kernelParams = [
      "nohibernate" # Disable hibernation
      "page_alloc.shuffle=1" # Enable page allocator randomization
      "page_poison=1" # Poison memory pages, wiping freed memory
      "pti=on" # Kernel page isolation
      "slab_nomerge" # Disable slab merging (Slab = chunk of memory)
      "slub_debug=FZP" # Enable sanity checks (F), redzoning (Z) and poisoning (P)
      "vsyscall=none" # vsyscall is obsolete
    ];

  };

  security = {
    forcePageTableIsolation = true;
    hideProcessInformation = true;
    lockKernelModules = true;
    protectKernelImage = true;
  };
}