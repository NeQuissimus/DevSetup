{
  config,
  lib,
  pkgs,
  ipv4Address,
  ...
}:

{
  programs.ssh = {
    agentTimeout = "4h";
    extraConfig = ''
      Host *
        ConnectTimeout 60
        ServerAliveInterval 240
        ConnectionAttempts 60
        PasswordAuthentication no
        ChallengeResponseAuthentication no
        PubkeyAuthentication yes
        UseRoaming no
    '';

    knownHosts = {
      github = {
        hostNames = [
          "github.com"
          "192.30.252.*"
          "192.30.253.*"
          "192.30.254.*"
          "192.30.255.*"
        ];
        publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
      };
    };

    startAgent = true;
  };

  services = {
    openssh = {
      allowSFTP = false;
      enable = lib.mkDefault false;

      extraConfig = ''
        ClientAliveInterval 300
        ClientAliveCountMax 2
      '';

      hostKeys = [
        {
          path = "/etc/ssh/ssh_host_ed25519_key";
          rounds = 127;
          type = "ed25519";
        }
        {
          bits = 4096;
          path = "/etc/ssh/ssh_host_rsa_key";
          type = "rsa";
        }
      ];

      listenAddresses = [
        {
          addr = ipv4Address;
          port = 22;
        }
      ];

      settings = {
        AllowUsers = [ "nequi" ];
        ChallengeResponseAuthentication = false;

        Ciphers = [
          "chacha20-poly1305@openssh.com"
          "aes256-gcm@openssh.com"
          "aes128-gcm@openssh.com"
        ];

        ClientAliveCountMax = 3;
        ClientAliveInterval = 300;
        HostbasedAuthentication = false;

        HostkeyAlgorithms = [
          "ssh-ed25519"
          "rsa-sha2-512"
          "rsa-sha2-256"
        ];

        IgnoreRhosts = true;
        KbdInteractiveAuthentication = false;

        KexAlgorithms = [
          "curve25519-sha256"
          "curve25519-sha256@libssh.org"
          "diffie-hellman-group16-sha512"
          "diffie-hellman-group18-sha512"
        ];

        LoginGraceTime = 30;

        MACs = [
          "hmac-sha2-512-etm@openssh.com"
          "hmac-sha2-256-etm@openssh.com"
          "umac-128-etm@openssh.com"
        ];

        MaxAuthTries = 3;
        MaxSessions = 2;
        MaxStartups = "10:30:100";
        PasswordAuthentication = false;
        PermitEmptyPasswords = false;
        PermitRootLogin = false;
      };
    };
  };
}
