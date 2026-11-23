{
  config,
  lib,
  pkgs,
  baseKernelPackages ? null,
  modlistFile ? null,
  ...
}:
let
  ff = x: lib.mkForce (lib.kernel.freeform x);
  fn = lib.mkForce lib.kernel.no;
  fy = lib.mkForce lib.kernel.yes;

  generatedConfig = lib.mkIf (modlistFile != null) (
    pkgs.stdenv.mkDerivation {
      name = "minimal-kernel-config";
      src = baseKernelPackages.kernel.src;

      nativeBuildInputs = with pkgs; [
        bison
        flex
        bc
        perl
        gawk
      ];

      buildPhase = ''
        # Start with a standard default config
        make defconfig

        # Format the module list to mimic /proc/modules for localmodconfig
        export LSMOD=$(mktemp)
        awk '{ print $1, 0, 0 }' ${modlistFile} > $LSMOD

        # Disable all modules not present in LSMOD
        yes "" | make localmodconfig

        # Optional: Force-enable critical NixOS modules (e.g., filesystems, virtualization)
        # scripts/config --module CONFIG_BTRFS_FS
        # scripts/config --enable CONFIG_overlay_FS

        cp .config $out
      '';

      dontInstall = true;
    }
  );

  customKernelPackages =
    if (modlistFile == null) then
      baseKernelPackages
    else
      pkgs.linuxPackagesFor (
        baseKernelPackages.kernel.override {
          configfile = generatedConfig;
          ignoreConfigErrors = true;
        }
      );

  modlist =
    if modlistFile != null then
      builtins.filter (line: line != "") (pkgs.lib.splitString "\n" (builtins.readFile modlistFile))
    else
      [ ];
in
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
      "dev.tty.ldisc_autoload" = 0; # Prevent loading vulnerable line disciplines
      "dev.tty.legacy_tiocsti" = 0; # Disable TIOCSTI which is used to inject keypresses

      "fs.protected_fifos" = 2; # Prevent FIFOs in world-writable environments
      "fs.protected_regular" = 2; # Prevent files in world-writable environments
      "fs.protected_symlinks" = 1; # Limit access to links
      "fs.protected_hardlinks" = 1; # Limit access to links
      "fs.suid_dumpable" = 0; # Make sure the default process dumpability is set

      "kernel.dmesg_restrict" = true; # Restrict dmesg access
      "kernel.kexec_load_disabled" = true; # Prevent kernel reload
      "kernel.kptr_restrict" = lib.mkForce 2; # Hide kernel pointers
      "kernel.oops_limit" = 100; # Reboot after 100 oops
      "kernel.perf_event_paranoid" = 3; # Restrict performance events
      "kernel.panic_on_oops" = true; # Kernel panic on oops
      "kernel.printk" = "3 3 3 3"; # Hide printing information leaks
      "kernel.randomize_va_space" = 2; # Enable all available Address Space Randomization (ASLR) for userspace processes
      "kernel.sysrq" = 4; # Users can only use the secure attention key
      "kernel.unprivileged_bpf_disabled" = true; # Prevent privilege escalation
      "kernel.unprivileged_userns_clone" = lib.mkDefault 0; # Restrict user namespaces
      "kernel.warn_limit" = 100; # Reboot after 100 WARN
      "kernel.yama.ptrace_scope" = 2; # Limit ptrace

      "net.core.bpf_jit_enable" = false; # Turn off bpf JIT
      "net.core.bpf_jit_harden" = 2; # Harden bpf JIT if it cannot be disabled

      "net.ipv4.conf.all.accept_redirects" = 0; # Don't accept redirects
      "net.ipv4.conf.all.accept_source_route" = 0; # Don't accept source routes
      "net.ipv4.conf.all.log_martians" = 1; # Log martian packets
      "net.ipv4.conf.all.rp_filter" = 1; # Protect against IP spoofing
      "net.ipv4.conf.all.secure_redirects" = 0; # No redirects
      "net.ipv4.conf.all.send_redirects" = 0; # No redirects (only needed on routers)
      "net.ipv4.conf.default.accept_redirects" = 0; # Don't accept redirects
      "net.ipv4.conf.default.accept_source_route" = 0; # Don't accept source routes
      "net.ipv4.conf.default.log_martians" = 1; # Log martian packets
      "net.ipv4.conf.default.rp_filter" = 1; # Protect against IP spoofing
      "net.ipv4.conf.default.secure_redirects" = 0; # No redirects
      "net.ipv4.conf.default.send_redirects" = 0; # No redirects (only needed on routers)
      "net.ipv4.icmp_echo_ignore_all" = 1; # Ignore ICMP requests
      "net.ipv4.tcp_dsack" = 0; # Disable TCP SACK
      "net.ipv4.tcp_fack" = 0; # Disable TCP SACK
      "net.ipv4.tcp_rfc1337" = 1; # Protect against tcp time-wait assassination hazards
      "net.ipv4.tcp_sack" = 0; # Disable TCP SACK
      "net.ipv4.tcp_syncookies" = 1; # Protect against SYN flood

      "net.ipv6.conf.all.accept_ra" = 0; # Don't accept router advertisements
      "net.ipv6.conf.all.accept_redirects" = 0; # Don't accept redirects
      "net.ipv6.conf.all.accept_source_route" = 0; # Don't accept source routes
      "net.ipv6.conf.all.secure_redirects" = 0; # No redirects
      "net.ipv6.conf.default.accept_ra" = 0; # Don't accept router advertisements
      "net.ipv6.conf.default.accept_redirects" = 0; # Don't accept redirects
      "net.ipv6.conf.default.accept_source_route" = 0; # Don't accept source routes
      "net.ipv6.conf.default.secure_redirects" = 0; # No redirects
      "net.ipv6.all.disable_ipv6" = 1; # Disable IPv6

      #      "user.max_user_namespaces" = 0; # Disable user namespaces, breaks Nix 2.0

      "vm.dirty_background_ratio" = 20; # Max % of RAM with dirty pages before reclaim
      "vm.dirty_ratio" = 30; # Max % of RAM with dirty pages before STW
      "vm.dirty_writeback_centisecs" = 500; # Frequency pages are reclaimed
      "vm.dirty_expire_centisecs" = 3000; # Max age of dirty pages
      "vm.drop_caches" = 1; # Drop caches early
      "vm.max_map_count" = 524240; # Increase page number
      "vm.mmap_min_addr" = 65536; # Enforce memory beyond NULL space
      "vm.mmap_rnd_bits" = 32; # Raise ASLR entropy
      "vm.mmap_rnd_compat_bits" = 16; # Raise ASLR entropy
      "vm.swappiness" = 1; # Minimum swap usage
      "vm.unprivileged_userfaultfd" = 0; # userfaultfd is used to exploit use-after-free
      "vm.vfs_cache_pressure" = 60; # Less reclaim pressure
    };

    kernelModules = lib.mkDefault modlist;

    kernelPackages = lib.mkIf (baseKernelPackages != null) customKernelPackages;

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
      #"oops=panic" # Prevent "oops" exploits
      ''quiet" "loglevel=0'' # Prevent information leak upon boot
      "page_alloc.shuffle=1" # Enable page allocator randomization
      "page_poison=1" # Overwrite freed pages
      "pti=on" # Kernel page isolation
      "randomize_kstack_offset=on" # Randomize kernel stack offset on syscall entry
      "slab_nomerge" # Disable slab merging (Slab = chunk of memory)
      "spectre_bhi=off"
      "spectre_v2_user=off"
      "spectre_v2_user=off"
      "slub_debug=FZ" # Enable sanity checks (F), redzoning (Z)
      "random.trust_cpu=off" # Enable or disable trusting the use of the CPU's random number generator
      "vdso32=0" # Make sure COMPAT_VDSO stays disabled
      "vsyscall=none" # vsyscall is obsolete
    ];

    kernelPatches = [
      {
        name = "hardened-config";
        patch = null;
        # $ kernel-hardening-checker -a -m show_fail
        structuredExtraConfig = {
          ACPI_CONFIGFS = fn;
          ACPI_TABLE_UPGRADE = fn;
          AIO = fn;
          ARCH_MMAP_RND_BITS = ff "32";
          ARCH_MMAP_RND_COMPAT_BITS = ff "16";
          BINFMT_MISC = fn;
          BLK_DEV_FD = fn;
          BLK_DEV_IO_TRACE = fn;
          BLK_DEV_UBLK = fn;
          BLK_DEV_WRITE_MOUNTED = fn;
          BPF_SYSCALL = fn;
          CACHESTAT_SYSCALL = fn;
          CFI_AUTO_DEFAULT = fn;
          CFI_CLANG = fy;
          CFI_PERMISSIVE = fy;
          CHECKPOINT_RESTORE = fn;
          COMPAT = fn;
          COREDUMP = fn;
          CRASH_DM_CRYPT = fn;
          CRASH_DUMP = fn;
          DEBUG_FS = fn;
          DEBUG_NOTIFIERS = fy;
          DEBUG_SG = fy;
          DEBUG_VIRTUAL = fy;
          DEVMEM = fn;
          DEVPORT = fn;
          EFI_CUSTOM_SSDT_OVERLAYS = fn;
          EFI_DISABLE_PCI_DMA = fn;
          FB = fn;
          FTRACE = fn;
          FUNCTION_TRACER = fn;
          GCC_PLUGIN_LATENT_ENTROPY = fy;
          GENERIC_TRACER = fn;
          HIBERNATION = fn;
          HIST_TRIGGERS = fn;
          HWPOISON_INJECT = fn;
          # IA32_EMULATION = fn; # Steam needs this
          INET_DIAG = fn;
          INIT_ON_FREE_DEFAULT_ON = fy;
          INTEL_IOMMU_DEFAULT_ON = fy;
          IOMMU_DEFAULT_DMA_STRICT = fy;
          IO_URING = fn;
          IP_SCTP = fn;
          KALLSYMS = fn;
          KCMP = fn;
          KEXEC_FILE = fn;
          KEXEC = fn;
          KPROBE_EVENTS = fn;
          KPROBES = fn;
          KSM = fn;
          KSTACK_ERASE = fy;
          KSTACK_ERASE_METRICS = fn;
          KSTACK_ERASE_RUNTIME_DISABLE = fn;
          LDISC_AUTOLOAD = fn;
          # LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY = fy; # Prevents kernel modules from loading
          MEM_SOFT_DIRTY = fn;
          MMIOTRACE = fn;
          MMIOTRACE_TEST = fn;
          MODIFY_LDT_SYSCALL = fn;
          # MODULE_FORCE_LOAD = fn;
          # MODULE_SIG_FORCE = fy;
          # MODULES = fy; # NO
          MPTCP = fn;
          MTD_PHRAM = fn;
          MTD_SLRAM = fn;
          N_GSM = fn;
          PAGE_TABLE_CHECK_ENFORCED = fy;
          PAGE_TABLE_CHECK = fy;
          PROC_KCORE = fn;
          PROC_MEM_NO_FORCE = fy;
          PROC_PAGE_MONITOR = fn;
          PROC_VMCORE = fn;
          PUNIT_ATOM_DEBUG = fn;
          RANDOM_KMALLOC_CACHES = fy;
          RANDSTRUCT_FULL = fy;
          RESET_ATTACK_MITIGATION = fy;
          RSEQ = fn;
          SECURITY_SELINUX_BOOTPARAM = fn;
          SECURITY_SELINUX_DEVELOP = fn;
          SLAB_MERGE_DEFAULT = fn;
          SMB_SERVER = fn;
          STACK_TRACER = fn;
          STAGING = fn;
          #  STATIC_USERMODEHELPER = fy; # No kernel modules loading
          SUNRPC_DEBUG = fn;
          TIPC = fn;
          #  TLS = fn; # Module for now
          #  TRIM_UNUSED_KSYMS = fy; # NVIDIA module won't build
          UBSAN_BOUNDS = fy;
          UBSAN_LOCAL_BOUNDS = fy;
          UBSAN_SANITIZE_ALL = fy;
          UBSAN_TRAP = fy;
          UPROBE_EVENTS = fn;
          USERFAULTFD = fn;
          USER_NS = fy; # NO
          VIDEO_VIVID = fn;
          #  VT = fn; # No boot
          WERROR = fy;
          X86_16BIT = fn;
          X86_CPUID = fn;
          X86_IOPL_IOPERM = fn;
          X86_MSR = fn;
          X86_INTEL_TSX_MODE_AUTO = fn;
          X86_INTEL_TSX_MODE_OFF = fy;
          X86_INTEL_TSX_MODE_ON = fn;
          X86_VSYSCALL_EMULATION = fn;
          X86_X32_ABI = fn;
          XFS_SUPPORT_V4 = fn;
          ZERO_CALL_USED_REGS = fy;
          ZSMALLOC_STAT = fn;

          RC_CORE = fn;
        };
      }
    ];
  };

  security = {
    forcePageTableIsolation = true;
    lockKernelModules = lib.mkDefault true;
    protectKernelImage = true;
  };
}
