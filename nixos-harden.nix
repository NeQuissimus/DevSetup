{ config, lib, pkgs, ... }:

with lib;

let
  kernelPkgs = pkgs.linuxPackages_latest_hardened;
  # kernelPkgs = import ./nixpkgs/grsec-4.9.nix { inherit pkgs; };
in {
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
      "iso9660" # Mount CDs
      "nls-cp437" # /boot
      "nls-iso8859-1" # /boot
      "vfat" # /boot
    ] ++ optionals (config.services.kbfs.enable) [
      "fuse" # KBFS
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
#      "user.max_user_namespaces" = 0; # Disable user namespaces, breaks Nix 2.0
      "vm.mmap_min_addr" = 65535; # Enforce memory beyond NULL space
      "vm.mmap_rnd_bits" = 32; # Raise ASLR entropy
    };

    kernelPackages = lib.mkDefault kernelPkgs;

    kernelParams = [
      "nohibernate" # Disable hibernation
      "page_poison=1" # Poison memory pages, wiping freed memory
      "slab_nomerge" # Disable slab merging (Slab = chunk of memory)
      "slub_debug=FZP" # Enable sanity checks (F), redzoning (Z) and poisoning (P)
      "vsyscall=none" # vsyscall is obsolete
    ];

    loader.systemd-boot.editor = false;
  };

  environment = {
    etc."ssh_moduli".text = (lib.fileContents ./etc/moduli);
  };

  networking = {
    firewall = {
      allowedTCPPorts = [ ];
      allowPing = false;
      enable = true;
    };
  };

  nix = {
    trustedBinaryCaches = [ http://hydra.nixos.org/ ];
    trustedUsers = [];
  };

  programs.ssh.extraConfig = ''
    Host *
      KexAlgorithms curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256
      PasswordAuthentication no
      ChallengeResponseAuthentication no
      PubkeyAuthentication yes
      HostKeyAlgorithms ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa
      Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-gcm@openssh.com,aes256-ctr,aes192-ctr,aes128-ctr
      MACs hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com
      UseRoaming no
  '';

  security = {
    apparmor.enable = true;
    hideProcessInformation = true;
    lockKernelModules = true;
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
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

    fail2ban = {
      daemonConfig = ''
        [sshd]
        maxretry = 3
        findtime = 43200
        bantime = 86400
      '';

      enable = true;
    };

    openssh = {
      allowSFTP = false;
      challengeResponseAuthentication = false;
      enable = mkDefault false;
      extraConfig = ''
        ClientAliveInterval 300
        ClientAliveCountMax 2
      '';
      hostKeys = [
        { path = "/etc/ssh/ssh_host_ed25519_key"; rounds = 127; type = "ed25519"; }
        { bits = 4096; path = "/etc/ssh/ssh_host_rsa_key"; type = "rsa"; }
      ];
      kexAlgorithms = [
        "curve25519-sha256@libssh.org"
        "diffie-hellman-group14-sha256"
        "diffie-hellman-group16-sha512"
        "diffie-hellman-group18-sha512"
      ];
      macs =  [
        "hmac-sha2-512-etm@openssh.com"
        "hmac-sha2-256-etm@openssh.com"
        "umac-128-etm@openssh.com"
      ];
      moduliFile = "/etc/ssh_moduli";
      passwordAuthentication = false;
      permitRootLogin = "no";
    };
  };

  system.autoUpgrade = {
    channel = mkDefault "https://nixos.org/channels/nixos-18.09";
    dates = "9:00";
    enable = mkDefault false;
  };

  users.users.root.shell = pkgs.nologin; # Make sure to have a user with a password :D
}
