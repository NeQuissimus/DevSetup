{ pkgs, config, lib, ... }:
{
  imports = [
    ../../extras/home.nix

    ./home/alacritty.nix
    ./home/emacs.nix
    ./home/git.nix
    ./home/xmonad.nix
    ./home/zsh.nix
  ];

  home = {
    file = {
      ".conky".source = ./conkyrc;
      ".config/nixpkgs/config.nix".source = ./config.nix;
      ".local/bin/nix-updates".source = ./nix-updates.sh;

      ".nanorc".text = ''
        set linenumbers
        set tabsize 2
        set tabstospaces
        set trimblanks
        set unix

        include ${pkgs.nanorc}/share/*.nanorc
      '';

      ".xinitrc".text = ''
        xrdb ~/.Xresources
        [[ -f ~/.Xdefaults ]] && xrdb -merge ~/.Xdefaults
        nix-shell -p xorg.xmodmap --command "xmodmap ~/.Xmodmap"
      '';

      ".Xmodmap".text = ''
        keycode 66 = Mode_switch Multi_key
        keycode 39 = s S ssharp
        keycode 38 = a A adiaeresis Adiaeresis
        keycode 30 = u U udiaeresis Udiaeresis
        keycode 32 = o O odiaeresis Odiaeresis
      '';

      ".Xresources".source = ./Xresources;
    };

    packages = with pkgs; [
      awscli
      bat
      exa
      firefox
      htop
      kubectl
      jq
      nano
      nyxt
      ripgrep
      slack
      xclip
    ];
  };

  news.display = "silent";

  programs = {
    alacritty.enable = true;

    emacs.enable = true;

    gpg.enable = true;

    git.enable = true;

    htop.enable = true;

    home-manager.enable = true;

    jq.enable = true;

    rofi = {
      enable = true;
      theme = "${pkgs.rofi}/share/rofi/themes/sidebar.rasi";
    };

    ssh = {
      enable = true;

      extraConfig = ''
        ChallengeResponseAuthentication no
        Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-gcm@openssh.com,aes256-ctr,aes192-ctr,aes128-ctr
        ConnectTimeout 60
        ConnectionAttempts 60
        HostKeyAlgorithms ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa
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

  xsession.windowManager.xmonad.enable = true;
}
