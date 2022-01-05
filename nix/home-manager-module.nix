{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.bevel;

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };

  toYamlFile = pkgs.callPackage ./to-yaml.nix { };

in
{
  options =
    {
      programs.bevel =
        {
          enable = mkEnableOption "Bevel cli and syncing";
          bevelReleasePackages =
            mkOption {
              description = "The bevelPackages attribute defined in the nix/overlay.nix file in the bevel repository.";
              default = (import ./pkgs.nix { }).bevelReleasePackages;
            };
          config =
            mkOption {
              description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
              default = { };
            };
          sync =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Bevel syncing";
                        server-url =
                          mkOption {
                            type = types.str;
                            example = "api.bevel.cs-syd.eu";
                            description = "The url of the sync server";
                          };
                        username =
                          mkOption {
                            type = types.str;
                            example = "syd";
                            description =
                              "The username to use when logging into the sync server";
                          };
                        password =
                          mkOption {
                            type = types.str;
                            example = "hunter12";
                            description =
                              "The password to use when logging into the sync server";
                          };
                      };
                  }
                );
            };
        };
    };
  config =
    let
      syncConfig = optionalAttrs (cfg.sync.enable or false) {
        server-url = cfg.sync.server-url;
        username = cfg.sync.username;
        password = cfg.sync.password;
      };

      syncBevelName = "sync-bevel";
      syncBevelService =
        {
          Unit =
            {
              Description = "Sync bevel";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "sync-bevel-service-ExecStart"
                  ''
                    exec ${cfg.bevelReleasePackages.bevel-cli}/bin/bevel sync
                  ''}";
              Type = "oneshot";
            };
        };
      syncBevelTimer =
        {
          Unit =
            {
              Description = "Sync bevel every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "daily";
              Persistent = true;
              Unit = "${syncBevelName}.service";
            };
        };

      bevelConfig =
        mergeListRecursively [
          syncConfig
          cfg.config
        ];

      # Convert the config file to pretty yaml, for readability.
      # The keys will not be in the "right" order but that's fine.
      bevelConfigFile = toYamlFile "bevel-config" bevelConfig;

      services =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncBevelName}" = syncBevelService;
          }
        );
      timers =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncBevelName}" = syncBevelTimer;
          }
        );
      packages =
        [
          cfg.bevelReleasePackages.bevel-cli
          cfg.bevelReleasePackages.bevel-gather
        ];


    in
    mkIf cfg.enable {
      xdg = {
        configFile."bevel/config.yaml".source = bevelConfigFile;
      };
      systemd.user =
        {
          startServices = true;
          services = services;
          timers = timers;
        };
      home.packages = packages;
    };
}
