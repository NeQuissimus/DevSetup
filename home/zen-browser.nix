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
        "browser.tabs.allow_transparent_browser" = {
          Value = true;
          Status = "locked";
        };
      };
    };

    profiles.default = {
      extensions.packages = with firefox-addons; [
        canvasblocker
        seventv
        sponsorblock
        twitch-auto-points
        ublock-origin
      ];

      pinsForce = true;
      pinsForceAction = "remove";
      pins = {
        "Grafana" = {
          id = "7113cf13-180b-4d04-be66-e9ad871cc631";
          url = "http://opi5plus.nequissimus.com";
        };
        "Router" = {
          id = "3e3a6298-c094-4e30-9201-740b82483129";
          url = "https://10.0.0.2";
        };
        "Home Assistant" = {
          id = "e7e6b15c-4708-43fb-a5ea-af2c6a58f678";
          url = "http://10.0.0.55:8123/dashboard-dashboard/0";
        };
        "Immich" = {
          id = "b59fd666-18f2-4749-8094-e185f6655f90";
          url = "http://10.102.0.37:2283/photos";
        };
        "Jellyfin" = {
          id = "aeb57ca3-15b2-492e-add4-5a3f48a36438";
          url = "http://10.102.0.37:8096/web";
        };
        "1Password" = {
          id = "1bbef0e3-dcad-4ad3-ae25-bad385808f27";
          url = "https://my.1password.com/home";
        };
        "Proton" = {
          id = "fbc2418e-f8f0-466d-a532-87bc57c01201";
          url = "https://account.proton.me/apps";
        };
        "F1TV" = {
          id = "7c938021-01c3-46d7-a104-492c4d3d140d";
          url = "https://f1tv.formula1.com";
        };
        "Twitch" = {
          id = "c8988540-0366-4d77-862c-fb84707c0306";
          url = "https://www.twitch.tv";
        };
      };

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

      spacesForce = true;

      spaces = {
        "Default" = {
          id = "1f4f055a-3495-423f-b159-0c482bbb4d4d";
          theme = {
            colors = [{
              red = 20;
              green = 20;
              blue = 20;
              type = "explicit-lightness";
              lightness = 20;
            }];
          };
        };
      };
    };

    setAsDefaultBrowser = true;
  };
}