{ config, lib, pkgs, ... }:

{
  programs.ssh = {
    agentTimeout = "4h";
    extraConfig = ''
      Host *
        ConnectTimeout 60
        ServerAliveInterval 240
        ConnectionAttempts 60
        KexAlgorithms curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256
        PasswordAuthentication no
        ChallengeResponseAuthentication no
        PubkeyAuthentication yes
        HostKeyAlgorithms ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa
        Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-gcm@openssh.com,aes256-ctr,aes192-ctr,aes128-ctr
        MACs hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com
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
        publicKey =
          "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
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

      settings = {
        KbdInteractiveAuthentication = false;

        KexAlgorithms = [
          "curve25519-sha256@libssh.org"
          "diffie-hellman-group14-sha256"
          "diffie-hellman-group16-sha512"
          "diffie-hellman-group18-sha512"
        ];

        Macs = [
          "hmac-sha2-512-etm@openssh.com"
          "hmac-sha2-256-etm@openssh.com"
          "umac-128-etm@openssh.com"
        ];

        PasswordAuthentication = false;
        PermitRootLogin = "no";
      };
    };
  };
}
