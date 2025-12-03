{ bevel-api-server
, opt-env-conf
}:
{ envname
}:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.bevel."${envname}";

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };
in
{
  options.services.bevel."${envname}" =
    {
      enable = mkEnableOption "Bevel Service";
      api-server = mkOption {
        type = types.submodule {
          options = {
            enable = mkEnableOption "Bevel API Server";
            hosts = mkOption {
              type = types.listOf types.str;
              default = [ ];
              example = "api.bevel.cs-syd.eu";
              description = "The host to serve api requests on";
            };
            openFirewall = mkOption {
              type = types.bool;
              default = false;
              description = "Whether to open the specified ports in the firewall";
            };
            config = mkOption {
              default = { };
              type = types.submodule {
                options = pkgs.callPackage ../bevel-api-server-gen/options.nix { };
              };
            };
            extraConfig = mkOption {
              description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
              default = { };
            };
          };
        };
      };
    };
  config =
    let
      working-dir = "/www/bevel/${envname}/";
      api-server-config = with cfg.api-server; mergeListRecursively [
        (builtins.removeAttrs cfg.api-server.config [ "override" "overrideDerivation" ])
        cfg.api-server.extraConfig
      ];
      api-server-config-file = (pkgs.formats.yaml { }).generate "bevel-api-server-config.yaml" api-server-config;
      api-server-working-dir = working-dir + "api-server/";
      api-server-service =
        with cfg.api-server;
        optionalAttrs enable {
          "bevel-api-server-${envname}" = opt-env-conf.addSettingsCheckToService { } {
            description = "Bevel API Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment = {
              "BEVEL_API_SERVER_CONFIG_FILE" = "${api-server-config-file}";
            };
            script = ''
              mkdir -p "${api-server-working-dir}"
              cd ${api-server-working-dir};
              ${bevel-api-server}/bin/bevel-api-server
            '';
            serviceConfig = {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
            unitConfig = {
              StartLimitIntervalSec = 0;
              # ensure Restart=always is always honoured
            };
          };
        };
      api-server-host = optionalAttrs (cfg.api-server.enable && cfg.api-server.hosts != [ ]) {
        "${head cfg.api-server.hosts}" = {
          enableACME = true;
          forceSSL = true;
          locations."/".proxyPass = "http://localhost:${builtins.toString cfg.api-server.config.port}";
          serverAliases = tail cfg.api-server.hosts;
        };
      };
    in
    mkIf cfg.enable {
      systemd.services = api-server-service;
      networking.firewall.allowedTCPPorts = optional
        ((cfg.api-server.enable or false) && cfg.api-server.openFirewall)
        cfg.api-server.config.port;
      services.nginx.virtualHosts = api-server-host;
    };
}
