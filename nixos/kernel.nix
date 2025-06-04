{ config, lib, pkgs, ... }:

{
  boot = {
    blacklistedKernelModules = lib.mkDefault [
      "adfs"
      "af_802154"
      "affs"
      "appletalk"
      "atm"
      "ax25"
      "befs"
      "bfs"
      "bluetoth"
      "btusb"
      "can"
      "cifs"
      "cramfs"
      "dccp"
      "decnet"
      "econet"
      "efs"
      "erofs"
      "exofs"
      "f2fs"
      "freevxfs"
      "gfs2"
      "hfs"
      "hfsplus"
      "hpfs"
      "ipx"
      "jffs2"
      "jfs"
      "minix"
      "n-hdlc"
      "netrom"
      "nfs"
      "nfsv3"
      "nfsv4"
      "nilfs2"
      "ntfs"
      "omfs"
      "p8022"
      "p8023"
      "psnap"
      "qnx4"
      "qnx6"
      "rds"
      "rose"
      "sctp"
      "squashfs"
      "sysv"
      "tipc"
      "udf"
      "ufs"
      "uvcvideo"
      "vivid"
      "x25"
    ];

    initrd.kernelModules = lib.mkDefault [
      "ahci"
      "aesni-intel"
      "iso9660" # Mount CDs
      "nls-cp437" # /boot
      "nls-iso8859-1" # /boot
      "tun"
      "usbhid"
      "vfat" # /boot
      "bridge" # Docker
      "br_netfilter" # Docker
      "nf_nat" # Docker
      "veth" # Docker
      "xt_conntrack" # Docker
      "xt_nat" # Docker
      "ip_vs" # Docker Swarm
      "vxlan" # Docker Swarm
    ];

    kernel.sysctl = {
      "abi.vsyscall32" = 0; # Disable VDSO for 32 bit syscalls
      "dev.tty.ldisc_autoload" =
        0; # Prevent loading vulnerable line disciplines
      "dev.tty.legacy_tiocsti" =
        0; # Disable TIOCSTI which is used to inject keypresses

      "fs.protected_fifos" = 2; # Prevent FIFOs in world-writable environments
      "fs.protected_regular" = 2; # Prevent files in world-writable environments
      "fs.protected_symlinks" = 1; # Limit access to links
      "fs.protected_hardlinks" = 1; # Limit access to links
      "fs.suid_dumpable" = 0; # Make sure the default process dumpability is set

      "kernel.dmesg_restrict" = true; # Restrict dmesg access
      "kernel.kexec_load_disabled" = true; # Prevent kernel reload
      "kernel.kptr_restrict" = lib.mkForce 2; # Hide kernel pointers
      "kernel.oops_limit" = 1; # Reboot after even 1 oops
      "kernel.perf_event_paranoid" = 3; # Restrict performance events
      "kernel.panic_on_oops" = true; # Kernel panic on oops
      "kernel.printk" = "3 3 3 3"; # Hide printing information leaks
      "kernel.randomize_va_space" =
        2; # Enable all available Address Space Randomization (ASLR) for userspace processes
      "kernel.sysrq" = 4; # Users can only use the secure attention key
      "kernel.unprivileged_bpf_disabled" = true; # Prevent privilege escalation
      "kernel.unprivileged_userns_clone" = 0; # Restrict user namespaces
      "kernel.warn_limit" = 1; # Reboot after even 1 WARN
      "kernel.yama.ptrace_scope" = 2; # Limit ptrace

      "net.core.bpf_jit_enable" = false; # Turn off bpf JIT
      "net.core.bpf_jit_harden" = 2; # Harden bpf JIT if it cannot be disabled

      "net.ipv4.conf.all.accept_redirects" = 0; # Don't accept redirects
      "net.ipv4.conf.all.accept_source_route" = 0; # Don't accept source routes
      "net.ipv4.conf.all.log_martians" = 1; # Log martian packets
      "net.ipv4.conf.all.rp_filter" = 1; # Protect against IP spoofing
      "net.ipv4.conf.all.secure_redirects" = 0; # No redirects
      "net.ipv4.conf.all.send_redirects" =
        0; # No redirects (only needed on routers)
      "net.ipv4.conf.default.accept_redirects" = 0; # Don't accept redirects
      "net.ipv4.conf.default.accept_source_route" =
        0; # Don't accept source routes
      "net.ipv4.conf.default.log_martians" = 1; # Log martian packets
      "net.ipv4.conf.default.rp_filter" = 1; # Protect against IP spoofing
      "net.ipv4.conf.default.secure_redirects" = 0; # No redirects
      "net.ipv4.conf.default.send_redirects" =
        0; # No redirects (only needed on routers)
      "net.ipv4.icmp_echo_ignore_all" = 1; # Ignore ICMP requests
      "net.ipv4.tcp_dsack" = 0; # Disable TCP SACK
      "net.ipv4.tcp_fack" = 0; # Disable TCP SACK
      "net.ipv4.tcp_rfc1337" =
        1; # Protect against tcp time-wait assassination hazards
      "net.ipv4.tcp_sack" = 0; # Disable TCP SACK
      "net.ipv4.tcp_syncookies" = 1; # Protect against SYN flood

      "net.ipv6.conf.all.accept_ra" = 0; # Don't accept router advertisements
      "net.ipv6.conf.all.accept_redirects" = 0; # Don't accept redirects
      "net.ipv6.conf.all.accept_source_route" = 0; # Don't accept source routes
      "net.ipv6.conf.all.secure_redirects" = 0; # No redirects
      "net.ipv6.conf.default.accept_ra" =
        0; # Don't accept router advertisements
      "net.ipv6.conf.default.accept_redirects" = 0; # Don't accept redirects
      "net.ipv6.conf.default.accept_source_route" =
        0; # Don't accept source routes
      "net.ipv6.conf.default.secure_redirects" = 0; # No redirects
      "net.ipv6.all.disable_ipv6" = 1; # Disable IPv6

      #      "user.max_user_namespaces" = 0; # Disable user namespaces, breaks Nix 2.0

      "vm.dirty_background_ratio" =
        20; # Max % of RAM with dirty pages before reclaim
      "vm.dirty_ratio" = 30; # Max % of RAM with dirty pages before STW
      "vm.dirty_writeback_centisecs" = 500; # Frequency pages are reclaimed
      "vm.dirty_expire_centisecs" = 3000; # Max age of dirty pages
      "vm.drop_caches" = 1; # Drop caches early
      "vm.max_map_count" = 524240; # Increase page number
      "vm.mmap_min_addr" = 65535; # Enforce memory beyond NULL space
      "vm.mmap_rnd_bits" = 32; # Raise ASLR entropy
      "vm.mmap_rnd_compat_bits" = 16; # Raise ASLR entropy
      "vm.swappiness" = 1; # Minimum swap usage
      "vm.unprivileged_userfaultfd" =
        0; # userfaultfd is used to exploit use-after-free
      "vm.vfs_cache_pressure" = 60; # Less reclaim pressure
    };

    kernelPackages = lib.mkDefault pkgs.linuxPackages_6_13_hardened;

    kernelParams = [
      "cfi=kcfi" # Disable FineIBT since it is weaker than pure KCFI
      "consoleblank=600" # Turn off display
      "debugfs=off" # No debugfs
      "hardened_usercopy=1" # Make sure CONFIG_HARDENED_USERCOPY stays enabled
      "iommu.passthrough=0" # Force IOMMU TLB invalidation so devices will never be able to access stale data contents
      "iommu.strict=1" # Force IOMMU TLB invalidation so devices will never be able to access stale data contents
      "init_on_alloc=1" # Zero memory on allocation
      "init_on_free=1" # Zero memory on free
      "kfence.sample_interval=100" # Enable kfence
      "nohibernate" # Disable hibernation
      "oops=panic" # Prevent "oops" exploits
      ''quiet" "loglevel=0'' # Prevent information leak upon boot
      "page_alloc.shuffle=1" # Enable page allocator randomization
      "pti=on" # Kernel page isolation
      "randomize_kstack_offset=on" # Randomize kernel stack offset on syscall entry
      "slab_nomerge" # Disable slab merging (Slab = chunk of memory)
      "slub_debug=FZ" # Enable sanity checks (F), redzoning (Z)
      "random.trust_cpu=off" # Enable or disable trusting the use of the CPU's random number generator
      "vdso32=0" # Make sure COMPAT_VDSO stays disabled
      "vsyscall=none" # vsyscall is obsolete
    ];
  };

  security = {
    forcePageTableIsolation = true;
    lockKernelModules = true;
    protectKernelImage = true;
  };
}
