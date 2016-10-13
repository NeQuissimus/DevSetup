# ~/.nixpkgs/config.nix

{
  allowUnfree = true;

  firefox = {
    enableGoogleTalkPlugin = true;
    enableAdobeFlash = true;
    enableBluejeans = true;
  };

  chromium = {
    enablePepperFlash = true;
    enablePepperPDF = true;
  };

  packageOverrides = pkgs_: with pkgs_; {
    user-nequi = with pkgs; buildEnv {
      name = "user-nequi";

      paths = [
        firefox
        gitFull
        gnupg
        gnupg1compat
        (import ../nixpkgs/httpstat.nix)
        keybase-go
        oh-my-zsh
        unzip
        vlc
        zip

        # Java
        jdk
        maven

        # Scala
        scala
        sbt
        activator

        (import ../nixpkgs/sublime3-dev.nix)
      ];
    };
  };
}

