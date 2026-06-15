{ config, lib, pkgs, ... }:

let
  domain = "opi5plus.nequissimus.com";
  scrapeThis = name: ip: port: {
    job_name = name;
    static_configs = [{ targets = [ "${ip}:${toString port}" ]; }];
  };
in {
  services = {
    grafana = {
      enable = true;

      settings = {
        analytics.reporting_enabled = false;

        security.secret_key = "SW2YcwTIb9zpOOhoPsMm";

        server = {
          http_addr = "127.0.0.1";
          http_port = 3000;
          enforce_domain = true;
          enable_gzip = true;
          inherit domain;
        };
      };

      provision = {
        datasources.settings.datasources = [
          {
            name = "Prometheus";
            type = "prometheus";
            url = "http://${config.services.prometheus.listenAddress}:${toString config.services.prometheus.port}";
            isDefault = true;
            editable = false;
          }
        ];

        enable = true;
      };
    };

    nginx.virtualHosts."${domain}" = {
      addSSL = true;
      enableACME = true;
      locations."/grafana/" = {
        proxyPass = "http://${toString config.services.grafana.settings.server.http_addr}:${toString config.services.grafana.settings.server.http_port}";
        proxyWebsockets = true;
        recommendedProxySettings = true;
      };
    };

    prometheus = {
      enable = true;

      exporters = {
        node = {
          enable = true;
          enabledCollectors = [ "systemd" ];
          port = 9002;
        };
      };

      globalConfig = {
        scrape_interval = "1m";
        scrape_timeout = "10s";
      };

      port = 9001;
      retentionTime = "30d";

      scrapeConfigs = [
        (scrapeThis "opi5plus" "127.0.0.1" 9002)
        (scrapeThis "opi5plus-dns" "127.0.0.1" 4000)
      ];
    };
  };
}