{ bevelReleasePackages }:
{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.bevel;

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };
in
{
  options = {
    programs.bevel = {
      enable = mkEnableOption "Bevel";
      bevelReleasePackages = mkOption {
        description = "The bevelPackages attribute defined in the nix/overlay.nix file in the bevel repository.";
        default = bevelReleasePackages;
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
            server-url = mkOption {
              type = types.str;
              example = "api.bevel.cs-syd.eu";
              description = "The url of the sync server";
            };
            username = mkOption {
              type = types.str;
              example = "syd";
              description =
                "The username to use when logging into the sync server";
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
              exec ${cfg.bevelReleasePackages.bevel-cli}/bin/bevel sync
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
        optionalAttrs (cfg.sync.enable or false) {
          "${syncBevelName}" = syncBevelTimer;
        }
      );
      packages = [
        cfg.bevelReleasePackages.bevel-cli
        cfg.bevelReleasePackages.bevel-gather # Needed for the harness
      ];

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
        source "${cfg.bevelReleasePackages.bevel-harness}/share/harness.bash"
        ${optionalString cfg.harness.bash.bindings "source ${cfg.bevelReleasePackages.bevel-harness}/share/bindings.bash"}
      '';

      programs.zsh.initExtra = mkIf (cfg.harness.zsh.enable) ''
        source "${cfg.bevelReleasePackages.bevel-harness}/share/harness.zsh"
        ${optionalString cfg.harness.zsh.bindings "source ${cfg.bevelReleasePackages.bevel-harness}/share/bindings.zsh"}
      '';
    };
}
