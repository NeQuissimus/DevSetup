{ pkgs, config, ... }:
{
  programs.chromium = {
    enable = true;

    extensions = [
      "cjpalhdlnbpafiamejdnhcphjbkeiagm"
      "gcbommkclmclpchllfjekcdonpmejbdp"
      "pkehgijcmpdhfbdbbnkijodmdjhbjlgp"
    ];

    homepageLocation = "about:config";
  };

  security.chromiumSuidSandbox.enable = true;
}
