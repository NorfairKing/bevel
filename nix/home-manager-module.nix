{ bevel-cli
, bevel-gather
, bevel-harness
, bevel-select
}:
{ lib
, pkgs
, config
, ...
}:

with lib;

let
  cfg = config.programs.bevel;

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };
in
{
  options = {
    programs.bevel = {
      enable = mkEnableOption "Bevel";
      bevel-cli = mkOption {
        description = "The bevel-cli package";
        default = bevel-cli;
      };
      bevel-gather = mkOption {
        description = "The bevel-gather package";
        default = bevel-gather;
      };
      bevel-harness = mkOption {
        description = "The bevel-harness package";
        default = bevel-harness;
      };
      bevel-select = mkOption {
        description = "The bevel-select package";
        default = bevel-select;
      };
      config = mkOption {
        description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
        default = { };
      };
      harness = {
        bash = {
          enable = mkEnableOption "Bevel Harness for bash";
          bindings = mkEnableOption "Bevel keybindings for bash";
        };
        zsh = {
          enable = mkEnableOption "Bevel Harness for zsh";
          bindings = mkEnableOption "Bevel keybindings for zsh";
        };
      };
      sync = mkOption {
        default = null;
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Bevel syncing";
            autosync = mkOption {
              type = types.bool;
              default = true;
              description = "Whether to sync automatically";
            };
            server-url = mkOption {
              type = types.str;
              example = "api.bevel.cs-syd.eu";
              description = "The url of the sync server";
            };
            username = mkOption {
              type = types.str;
              example = "syd";
              description = "The username to use when logging into the sync server";
            };
            password = mkOption {
              type = types.nullOr types.str;
              default = null;
              example = "hunter12";
              description = "The password to use when logging into the sync server";
            };
            password-file = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "The file with the password to use when logging into the sync server";
            };
          };
        });
      };
    };
  };
  config =
    let
      syncConfig = optionalAttrs (cfg.sync.enable or false) {
        server-url = cfg.sync.server-url;
        username = cfg.sync.username;
        password = cfg.sync.password;
        password-file = cfg.sync.password-file;
      };

      syncBevelName = "sync-bevel";
      syncBevelService = {
        Unit = {
          Description = "Sync bevel";
          Wants = [ "network-online.target" ];
        };
        Service = {
          ExecStart = "${pkgs.writeShellScript "sync-bevel-service-ExecStart" ''
              exec ${cfg.bevel-cli}/bin/bevel sync
            ''}";
          Type = "oneshot";
        };
      };
      syncBevelTimer = {
        Unit = {
          Description = "Sync bevel every day";
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
        Timer = {
          OnCalendar = "daily";
          Persistent = true;
          Unit = "${syncBevelName}.service";
        };
      };

      bevelConfig = mergeListRecursively [
        syncConfig
        cfg.config
      ];

      # Convert the config file to pretty yaml, for readability.
      # The keys will not be in the "right" order but that's fine.
      bevelConfigFile = (pkgs.formats.yaml { }).generate "bevel-config.yaml" bevelConfig;

      services = (
        optionalAttrs (cfg.sync.enable or false) {
          "${syncBevelName}" = syncBevelService;
        }
      );
      timers = (
        optionalAttrs ((cfg.sync.enable or false) && (cfg.sync.autosync or false)) {
          "${syncBevelName}" = syncBevelTimer;
        }
      );
      packages = [
        cfg.bevel-gather # Needed for the harness
        cfg.bevel-select
      ] ++ optional (cfg.sync.enable or false) cfg.bevel-cli;

    in
    mkIf cfg.enable {
      xdg = {
        configFile."bevel/config.yaml".source = bevelConfigFile;
      };
      systemd.user = {
        startServices = true;
        services = services;
        timers = timers;
      };
      home.packages = packages;

      programs.bash.initExtra = mkIf (cfg.harness.bash.enable) ''
        source "${pkgs.bash-preexec}/share/bash/bash-preexec.sh"
        source "${cfg.bevel-harness}/share/harness.bash"
        ${optionalString cfg.harness.bash.bindings "source ${cfg.bevel-harness}/share/bindings.bash"}
      '';

      programs.zsh.initExtra = mkIf (cfg.harness.zsh.enable) ''
        source "${cfg.bevel-harness}/share/harness.zsh"
        ${optionalString cfg.harness.zsh.bindings "source ${cfg.bevel-harness}/share/bindings.zsh"}
      '';
    };
}
