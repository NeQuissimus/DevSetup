{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    autosuggestions = {
      enable = true;
      extraConfig = {
        "ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE" = "20";
        "ZSH_AUTOSUGGEST_USE_ASYNC" = "true";
      };
    };
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
