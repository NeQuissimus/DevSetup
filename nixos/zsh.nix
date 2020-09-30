{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    autosuggestions.enable = true;
    enable = true;
    enableCompletion = true;

    ohMyZsh = {
      customPkgs = [ pkgs.spaceship-prompt ];
      enable = true;
    };

    syntaxHighlighting = {
      enable = true;

      highlighters = [ "main" "brackets" "pattern" "root" "line" ];
    };
  };

  users.defaultUserShell = "${pkgs.zsh}/bin/zsh";
}
