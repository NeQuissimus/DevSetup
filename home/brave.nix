{ config, pkgs, ... }:
{
  programs.chromium = {
    commandLineArgs = [
      "--disable-features=AutofillSavePaymentMethods"
    ];

    enable = true;

    extensions = [
      { id = "ammjkodgmmoknidbanneddgankgfejfh"; } # 7TV
      { id = "mnjggcdmjocbbbhaepdhchncahnbgone"; } # SponsorBlock for YouTube
      { id = "jdpblpklojajpopllbckephjndibljbc"; } # Twitch Channel Points Auto Clicker
      { id = "caldgcgjbhdhmiaomefkhknhhgodhbin"; } # Vertical Twitch
    ];

    package = pkgs.brave-origin;
  };
}
