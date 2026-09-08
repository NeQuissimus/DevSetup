{ inputs, ... }:
{
  imports = [
    inputs.helium-flake.nixosModules.default
  ];

  programs.helium = {
    enable = true;

    flags = [
      "--disable-features=AutofillSavePaymentMethods"
      "--ozone-platform-hint=auto"
      "--disable-gpu"
    ];

    policies = {
      "BrowserSignin" = 0;
      "PasswordManagerEnabled" = false;
      "SyncDisabled" = true;
      "DefaultSearchProviderEnabled" = true;
      "DefaultSearchProviderSearchURL" = "https://ecosia.org/search?method=index&q={searchTerms}";
      "ExtensionInstallForcelist" = [
        "ammjkodgmmoknidbanneddgankgfejfh" # 7TV
        "mnjggcdmjocbbbhaepdhchncahnbgone" # SponsorBlock for YouTube
        "jdpblpklojajpopllbckephjndibljbc" # Twitch Channel Points Auto Clicker
        "caldgcgjbhdhmiaomefkhknhhgodhbin" # Vertical Twitch
      ];
      "DnsOverHttpsMode" = "off";
      "IncognitoModeAvailability" = 1;
      "ManagedBookmarks" = [
        {
          "name" = "1Password";
          "url" = "https://my.1password.com/app";
        }
        {
          "name" = "F1TV";
          "url" = "https://f1tv.formula1.com/";
        }
        {
          "name" = "Grafana";
          "url" = "http://opi5plus.nequissimus.com/dashboards";
        }
        {
          "name" = "Home Assistant";
          "url" = "http://10.0.0.55:8123/dashboard-dashboard/0";
        }
        {
          "name" = "Immich";
          "url" = "http://10.102.0.37:2283/photos";
        }
        {
          "name" = "Jellyfin";
          "url" = "http://10.102.0.37:8096/web/#/home";
        }
        {
          "name" = "Proton";
          "url" = "https://account.proton.me/apps";
        }
        {
          "name" = "Twitch";
          "url" = "https://www.twitch.tv/";
        }
        {
          "name" = "Unifi OS";
          "url" = "https://10.0.0.2/network/default/dashboard";
        }
        {
          "name" = "YouTube";
          "url" = "https://www.youtube.com/feed/subscriptions";
        }
      ];
      "MetricsReportingEnabled" = false;
      "SearchSuggestEnabled" = false;
    };
  };
}
