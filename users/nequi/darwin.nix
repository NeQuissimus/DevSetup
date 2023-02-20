{ pkgs, config, lib, ... }:
{
  imports = [
#    ./home/emacs.nix
    ./home/git.nix
    ./home/zsh.nix

      (
        let
          declCachix = builtins.fetchTarball "https://github.com/jonascarpay/declarative-cachix/archive/a2aead56e21e81e3eda1dc58ac2d5e1dc4bf05d7.tar.gz";
        in import "${declCachix}/home-manager.nix"
      )
  ];

  caches.cachix = [
    "nequissimus"
    "nix-community"
  ];

  home = {
    file = {
      ".config/kitty/kitty.conf".text = ''
        # vim:fileencoding=utf-8:foldmethod=marker
        background #111111
        background_opacity 0.9
        font_family      Hasklig
        font_size 12.0
        scrollback_lines 20000
        detect_urls yes
        strip_trailing_spaces always
        enable_audio_bell no
        window_alert_on_bell no
        macos_quit_when_last_window_closed yes
        macos_thicken_font 0.5

        map shift+left send_text all \x1b\x62
        map shift+right send_text all \x1b\x66

        map ctrl+f launch --type=overlay --stdin-source=@screen_scrollback ${pkgs.fzf}/bin/fzf --no-sort --no-mouse -i --tac --exact
      '';

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
      coreutils
      bat
#      exa
      fzf
#      gnused
      htop
      jq
#      kt
      nano
#      pandoc
#      procps
      ripgrep
    ];

    username = "nequi";
    stateVersion = "22.05";
  };

  news.display = "silent";

  programs = {
    emacs.enable = true;

   git = {
      enable = true;
#      extraConfig = {
#        credential.helper = "store --file /opt/dev/var/private/git_credential_store";
#        url."https://github.com/Shopify/".insteadOf = [ "git@github.com:Shopify/" "git@github.com:shopify/" "ssh://git@github.com/Shopify/" "ssh://git@github.com/shopify/" ];
#      };
#      signing = {
#        key = "058F3C6EC7452F0015428BE13F563A8A6F0D693F";
#        gpgPath = "/opt/dev/bin/gpg-auto-pin";
#      };
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
