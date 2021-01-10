{ config, lib, pkgs, ... }:

with lib;

let
  protocolF = type: ports: {
    type = mkOption {
      type = types.str;
      default = type;
    };

    ports = mkOption {
      type = types.listOf types.int;
      default = ports;
    };

    enable = mkEnableOption type;
  };

  protocol2str = p: (optionalString p.enable ''
  - type: ${p.type}
    ports: [${concatStringsSep ", " (map toString p.ports)}]
  '');

  cfg = config.services.packetbeat;

  packetbeatYml = pkgs.writeText "packetbeat.yml" (''
    name: ${cfg.name}
    tags: ${builtins.toJSON cfg.tags}
  ''
  + optionalString (any (p: p.enable) (attrValues cfg.protocols)) ''
    packetbeat.protocols:
  ''
  + concatStringsSep "\n\n" (map protocol2str (attrValues cfg.protocols))
  + ''
    ${cfg.extraConfig}
  '');

in
{
  options = {

    services.packetbeat = {

      enable = mkEnableOption "packetbeat";

      package = mkOption {
        type = types.package;
        default = pkgs.packetbeat;
        defaultText = "pkgs.packetbeat";
        example = literalExample "pkgs.packetbeat7";
        description = ''
          The packetbeat package to use
        '';
      };

      name = mkOption {
        type = types.str;
        default = "packetbeat";
        description = "Name of the beat";
      };

      tags = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Tags to place on the shipped log messages";
      };

      stateDir = mkOption {
        type = types.str;
        default = "packetbeat";
        description = ''
          Directory below <literal>/var/lib/</literal> to store packetbeat's
          own logs and other data. This directory will be created automatically
          using systemd's StateDirectory mechanism.
        '';
      };

      flows = {
        enable = mkEnableOption "flows";
        period = mkOption {
          type = types.str;
          default = "10s";
          description = "Flow period";
        };
        timeout = mkOption {
          type = types.str;
          default = "30s";
          description = "Flow timeout";
        };
      };

      protocols = {
        dhcpv4 = protocolF "dhcpv4" [67 68];
        dns = protocolF "dns" [53];
        http = protocolF "http"[80 8080 8000 5000 8002];
        amqp = protocolF "amqp" [5672];
        cassandra = protocolF "cassandra"[9042];
        memcache = protocolF "memcache" [11211];
        mysql = protocolF "mysql"[3306 3307];
        redis = protocolF "redis" [6379];
        psql = protocolF "pgsql"[5432];
        thrift = protocolF "thrift" [9090];
        tls = protocolF "tls" [443 993 995 5223 8443 8883 9243];
      };

      extraConfig = mkOption {
        type = types.lines;
        default = '''';
        description = "Any other configuration options you want to add";
      };

    };
  };

  config = mkIf cfg.enable {

    assertions = [
      {
        assertion = !hasPrefix "/" cfg.stateDir;
        message =
          "The option services.packetbeat.stateDir shouldn't be an absolute directory." +
          " It should be a directory relative to /var/lib/.";
      }
    ];

    systemd.services.packetbeat = {
      description = "Packetbeat log shipper";
      wantedBy = [ "multi-user.target" ];
      preStart = ''
        mkdir -p ${cfg.stateDir}/data
        mkdir -p ${cfg.stateDir}/logs
      '';
      serviceConfig = {
        StateDirectory = cfg.stateDir;
        ExecStart = ''
          ${cfg.package}/bin/packetbeat \
            -c ${packetbeatYml} \
            -path.data /var/lib/${cfg.stateDir}/data \
            -path.logs /var/lib/${cfg.stateDir}/logs'';
        Restart = "always";
      };
    };
  };
}
