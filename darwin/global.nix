{ pkgs, lib, ... }: {
  environment.systemPackages = with pkgs; [ ];

  fonts.packages = with pkgs; [ fira-code ];

  nix = {
    configureBuildUsers = true;

    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
      interval = {
        Hour = 6;
        Minute = 0;
      };
    };

    settings = {
      experimental-features = "nix-command flakes";
      trusted-users = [ "nequi" ];
    };
  };

  nixpkgs.config.allowUnfree = true;

  programs.gnupg.agent.enable = true;

  services.nix-daemon.enable = true;

  system.defaults = {
    dock = {
      minimize-to-application = true;

      orientation = "right";

      persistent-apps = [
        "/Users/nequi/Applications/Home Manager Apps/WezTerm.app"
        "/Applications/Google Chrome.app"
        "/Users/nequi/Applications/Home Manager Apps/Visual Studio Code.app"
        "/Applications/Slack.app"
      ];

      persistent-others = [ "/Users/nequi/Downloads" ];
      show-recents = false;
      wvous-tl-corner = 1;
      wvous-bl-corner = 1;
      wvous-tr-corner = 1;
      wvous-br-corner = 13; # Lock screen
    };

    finder.ShowPathbar = true;

    NSGlobalDomain."com.apple.swipescrolldirection" = false;
  };

  users.users.nequi = {
    name = "nequi";
    home = "/Users/nequi";
  };
}
