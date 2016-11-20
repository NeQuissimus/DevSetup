# ~/.nixpkgs/config.nix

{
  allowUnfree = true;

  firefox = {
    enableGoogleTalkPlugin = true;
    enableAdobeFlash = true;
  };

  chromium = {
    enablePepperFlash = true;
    enablePepperPDF = true;
  };

  packageOverrides = pkgs_: with pkgs_; {
    user-basics = with pkgs; buildEnv {
      name = "user-basics";

      paths = [
        dejavu_fonts
        firefox
        gitFull
        gnupg
        gnupg1compat
        httpstat
        keybase-go
        oh-my-zsh
        unzip
        zip
      ];
    };

    user-haskell = with pkgs; buildEnv {
      name = "user-haskell";

      paths = [
        ghc
      ];
    };

    user-java = with pkgs; buildEnv {
      name = "user-java";

      paths = [
        gradle
        maven
        openjdk8
      ];
    };

    user-scala = with pkgs; buildEnv {
      name = "user-scala";

      paths = [
        activator
        ammonite-repl
        sbt
        scala
      ];
    };

    sublime3-dev = (import ../nixpkgs/sublime3-dev.nix);

    user-misc = with pkgs; buildEnv {
      name = "user-misc";

      paths = [
        chromium
        encfs
        franz
        irssi
        jekyll
        nodejs
        ruby
        texlive.combined.scheme-full
        vlc
        wine
      ];
    };
  };
}

