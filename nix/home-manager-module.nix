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
        default = { };
        type = types.submodule {
          options = pkgs.callPackage ../bevel-cli/options.nix { };
        };
      };
      extraConfig = mkOption {
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
      sync.enable = mkEnableOption "Bevel syncing";
      sync.autosync = mkEnableOption "Sync automatically";
    };
  };
  config =
    let
      syncBevelName = "bevel-sync";
      syncBevelService = {
        Unit = {
          Description = "Sync bevel";
          Wants = [ "network-online.target" ];
        };
        Service = {
          ExecStart = "${pkgs.writeShellScript "bevel-sync-service-ExecStart" ''
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
        (builtins.removeAttrs cfg.config [ "override" "overrideDerivation" ])
        cfg.extraConfig
      ];

      # Convert the config file to pretty yaml, for readability.
      # The keys will not be in the "right" order but that's fine.
      bevelConfigFile = (pkgs.formats.yaml { }).generate "bevel-config.yaml" bevelConfig;

      services = optionalAttrs (cfg.sync.enable or false) {
        "${syncBevelName}" = syncBevelService;
      };
      timers = optionalAttrs ((cfg.sync.enable or false) && (cfg.sync.autosync or false)) {
        "${syncBevelName}" = syncBevelTimer;
      };
      packages = [
        cfg.bevel-gather # Needed for the harness
        cfg.bevel-select
      ] ++ optional (cfg.sync.enable or false) cfg.bevel-cli;

    in
    mkIf cfg.enable {
      # We have no "opt-env-conf"-style settings check because the 'password'
      # secret is provided by agenix' home-manager service, which uses a
      # systemd service instead of an activation script.

      home.packages = packages;
      xdg.configFile = {
        "bevel/config.yaml".source = bevelConfigFile;
      };
      systemd.user = {
        startServices = true;
        inherit services;
        inherit timers;
      };

      programs.bash.initExtra = mkIf (cfg.harness.bash.enable) ''
        source "${pkgs.bash-preexec}/share/bash/bash-preexec.sh"
        source "${cfg.bevel-harness}/share/harness.bash"
        ${optionalString cfg.harness.bash.bindings "source ${cfg.bevel-harness}/share/bindings.bash"}
      '';

      programs.zsh.initContent = mkIf (cfg.harness.zsh.enable) ''
        source "${cfg.bevel-harness}/share/harness.zsh"
        ${optionalString cfg.harness.zsh.bindings "source ${cfg.bevel-harness}/share/bindings.zsh"}
      '';
    };
}
