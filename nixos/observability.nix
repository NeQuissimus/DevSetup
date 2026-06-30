{
  config,
  lib,
  pkgs,
  ...
}:

let
  domain = "opi5plus.nequissimus.com";
  scrapeThis = name: ip: port: {
    job_name = name;
    static_configs = [ { targets = [ "${ip}:${toString port}" ]; } ];
  };
in
{
  environment.etc."grafana-blocky/blocky.json".source = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/0xERR0R/blocky/refs/tags/v${pkgs.blocky.version}/docs/blocky-grafana.json";
    sha256 = "1z04dh3ijnq62bii64vxp4681i8ssl419vj1471dmm7n8w8wvzbz";
  };

  networking = {
    firewall.allowedTCPPorts = [ 80 ];

    hosts = {
      "10.0.0.54" = [ domain ];
    };
  };

  services = {
    grafana = {
      declarativePlugins = with pkgs.grafanaPlugins; [
        frser-sqlite-datasource
      ];

      enable = true;

      settings = {
        analytics.reporting_enabled = false;

        security.secret_key = "SW2YcwTIb9zpOOhoPsMm";

        server = {
          http_addr = "127.0.0.1";
          http_port = 3000;
          enable_gzip = true;
          inherit domain;
        };
      };

      provision = {
        dashboards.settings.providers = [
          {
            name = "Blocky";
            disableDeletion = true;
            options = {
              path = "/etc/grafana-blocky";
              foldersFromFilesStructure = true;
            };
          }
        ];

        datasources.settings.datasources = [
          {
            name = "Prometheus";
            type = "prometheus";
            url = "http://${config.services.prometheus.listenAddress}:${toString config.services.prometheus.port}";
            isDefault = true;
            editable = false;
            jsonData = {
              timeInterval = "1m";
            };
          }
        ]
        ++ (lib.optionals config.services.blocky.enable [
          {
            name = "blocky-sqlite";
            type = "frser-sqlite-datasource";
            isDefault = false;
            editable = false;
            access = "proxy";
            jsonData = {
              path = config.services.blocky.settings.queryLog.target;
            };

          }
        ]);

        enable = true;
      };
    };

    nginx = {
      enable = true;

      virtualHosts."${config.services.grafana.settings.server.domain}" = {
        locations."/" = {
          extraConfig = ''
            proxy_set_header Host ${config.services.grafana.settings.server.domain};
          '';

          proxyPass = "http://${toString config.services.grafana.settings.server.http_addr}:${toString config.services.grafana.settings.server.http_port}";
          proxyWebsockets = true;
        };
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
      retentionTime = "14d";

      scrapeConfigs = [
        (scrapeThis "opi5plus" "127.0.0.1" 9002)
        (scrapeThis "opi5plus-dns" "127.0.0.1" 4000)
        (scrapeThis "rpi4b-dns" "10.0.0.53" 4000)
      ];
    };
  };
}
