{ config, pkgs, ... }:
let
  nequi-zsh = pkgs.stdenv.mkDerivation rec {
    pname = "nequi-zsh";
    version = "1.1";

    src = pkgs.fetchFromGitHub {
      owner = "NeQuissimus";
      repo = "nequi-zsh";
      sha256 = "sha256-vlcBsRsMy5GiKO4egpX3vUS8EJTLr2UWLJF1az7rHA8=";
      rev = "v${version}";
    };

    installPhase = ''
      mkdir -p "$out/share/zsh/themes/"
      install -Dm0644 themes/nequissimus.zsh-theme "$out/share/zsh/themes/"
    '';
  };
in {
  programs.zsh = {
    autosuggestions.enable = true;
    enable = true;
    enableCompletion = true;

    ohMyZsh = {
      customPkgs = [ nequi-zsh ];
      enable = true;
      plugins = [ "docker" ];
      theme = "nequissimus";
    };

    syntaxHighlighting.enable = true;
  };
}
