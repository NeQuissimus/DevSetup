{ pkgs, ... }:
let
  username = "nequi";
in {
  fonts.fontconfig = {
    defaultFonts.monospace = [ "Fira Mono" ];
    enable = true;
  };

  home = {
    inherit username;

    file.".nanorc".text = ''
      set linenumbers
      set tabsize 2
      set tabstospaces
      set trimblanks
      set unix

      include ${pkgs.nanorc}/share/*.nanorc
    '';

    homeDirectory = "/home/${username}";

    packages = with pkgs; [
      nerd-fonts.fira-code
    ];

    stateVersion = "26.05";
  };

  manual = {
    html.enable = false;
    manpages.enable = false;
    json.enable = false;
  };

  news.display = "silent";

  programs = {
    bat.enable = true;

    eza = {
      enable = true;
      icons = "auto";
      git = true;
    };

    fzf = {
      enable = true;
      enableZshIntegration = true;
      defaultCommand = "${pkgs.fd}/bin/fd --type f";
      defaultOptions = [
        "--border"
        "--inline-info"
      ];
    };

    htop = {
      enable = true;
      settings.show_program_path = true;
    };

    jq.enable = true;

    ripgrep.enable = true;

    zoxide = {
      enable = true;
      enableZshIntegration = true;
      options = [
        "--cmd"
        "cd"
      ];
    };
  };
}