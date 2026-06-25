{ inputs, pkgs, lib, ...}:
let
  firefox-addons = inputs.firefox-addons.packages.${pkgs.stdenv.hostPlatform.system};
in {
  imports = [
    inputs.zen-browser.homeModules.beta
  ];

  programs.zen-browser = {
    enable = true;

    policies = {
      AutofillAddressEnabled = false;
      AutofillCreditCardEnabled = false;
      DisableAppUpdate = true;
      DisableFeedbackCommands = true;
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DNSOverHTTPS = false;
      DontCheckDefaultBrowser = true;
      HttpsOnlyMode = true;
      OfferToSaveLogins = false;
      EnableTrackingProtection = {
        Value = true;
        Locked = true;
        Cryptomining = true;
        Fingerprinting = true;
      };

      Preferences = {
        "browser.startup.homepage" = {
          Value = "about:blank";
          Status = "locked";
        };
        "browser.tabs.warnOnClose" = {
          Value = false;
          Status = "locked";
        };
        "zen.view.hide-window-controls" = {
          Value = false;
          Status = "locked";
        };
        "zen.view.use-single-toolbar" = {
          Value = false;
          Status = "locked";
        };
      };
    };

    profiles.default = {
      bookmarks = {
        force = true;

        settings = [
          {
            name = "Local";
            toolbar = true;
            bookmarks = [
              {
                name = "Grafana";
                url = "http://opi5plus.nequissimus.com";
              }
              {
                name = "Router";
                url = "https://10.0.0.2";
              }
              {
                name = "Home Assistant";
                url = "http://10.0.0.55:8123/dashboard-dashboard/0";
              }
              {
                name = "Immich";
                url = "http://10.102.0.37:2283/photos";
              }
              {
                name = "Jellyfin";
                url = "http://10.102.0.37:8096/web";
              }
              {
                name = "1Password";
                url = "https://my.1password.com/home";
              }
              {
                name = "Proton";
                url = "https://account.proton.me/apps";
              }
              {
                name = "F1TV";
                url = "https://f1tv.formula1.com/";
              }
              {
                name = "Twitch";
                url = "https://www.twitch.tv/";
              }
            ];
          }
        ];
      };

      extensions.packages = with firefox-addons; [
        canvasblocker
        seventv
        twitch-auto-points
        ublock-origin
      ];

      search = {
        default = "ecosia";

        engines = {
          ecosia = {
            urls = [{
              template = "https://www.ecosia.org/search?q={searchTerms}";
              params = [{
                name = "query";
                value = "searchTerms";
              }];
            }];
          };
        };

        force = true;
      };

      settings = {
        "zen.welcome-screen.seen" = true;
      };
    };

    setAsDefaultBrowser = true;
  };
}