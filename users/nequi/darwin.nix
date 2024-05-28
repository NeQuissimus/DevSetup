{ pkgs, config, lib, ... }:
{
  imports = [
    ./home/git.nix
    ./home/zsh.nix
  ];

  home = {
    file = {
      ".nanorc".text = ''
        set linenumbers
        set tabsize 2
        set tabstospaces
        set trimblanks
        set unix

        include ${pkgs.nanorc}/share/*.nanorc
      '';

      ".skhdrc".source = ./skhdrc;
      ".yabairc".source = ./yabairc;
    };

    homeDirectory = "/Users/nequi";

    packages = with pkgs; [
      fzf
      htop
      jq
      nano
      ripgrep
    ];

    username = "nequi";
    stateVersion = "22.05";
  };

  manual = {
    html.enable = false;
    manpages.enable = false;
    json.enable = false;
  };

  news.display = "silent";

  programs = {
   git = {
      enable = true;
      extraConfig = {
        credential.helper = "store --file /opt/dev/var/private/git_credential_store";
        url."https://github.com/Shopify/".insteadOf = [ "git@github.com:Shopify/" "git@github.com:shopify/" "ssh://git@github.com/Shopify/" "ssh://git@github.com/shopify/" ];
      };
      signing = {
        key = "F09D70D880F01FF54E51477B06A338C29C3CE904";
        gpgPath = "/opt/dev/bin/gpg-auto-pin";
      };
      userEmail = "tim@nequissimus.com";
    };

    htop.enable = true;

    home-manager.enable = true;

    jq.enable = true;

    ssh = {
      enable = true;

      extraConfig = ''
        ChallengeResponseAuthentication no
        Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-gcm@openssh.com,aes256-ctr,aes192-ctr,aes128-ctr
        ConnectTimeout 60
        ConnectionAttempts 60
        HostKeyAlgorithms ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa,rsa-sha2-512
        KexAlgorithms curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256
        MACs hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com
        PasswordAuthentication no
        PubkeyAuthentication yes
        UseRoaming no
      '';

      hashKnownHosts = true;

      serverAliveInterval = 240;
    };

    zsh.enable = true;
  };
}
