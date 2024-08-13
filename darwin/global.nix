{ pkgs, lib, ... }: {
  nix = {
    configureBuildUsers = true;
    settings = {
      experimental-features = "nix-command flakes";
      trusted-users = [ "nequi" ];
    };
  };

  users.users.nequi = {
    name = "nequi";
    home = "/Users/nequi";
  };

  nix.gc = {
    automatic = true;
    options = "--delete-older-than 7d";
    interval = {
      Hour = 2;
      Minute = 0;
    };
  };

  services.nix-daemon.enable = true;

  environment.systemPackages = with pkgs; [ gnupg ];

  fonts.packages = with pkgs; [ fira-code ];

  programs.gnupg.agent.enable = true;
}
