{ lib }:
{
  database = lib.mkOption {
    default = null;
    description = "database file";
    type = lib.types.nullOr lib.types.str;
  };
  necrork = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        intray = lib.mkOption {
          default = { };
          type = lib.types.submodule {
            options = {
              key = lib.mkOption {
                default = null;
                description = "Access key";
                type = lib.types.nullOr lib.types.str;
              };
              username = lib.mkOption {
                default = null;
                description = "Username";
                type = lib.types.nullOr lib.types.str;
              };
            };
          };
        };
        notifier = lib.mkOption {
          default = { };
          type = lib.types.submodule {
            options = {
              enable = lib.mkOption {
                default = null;
                description = "enable the notifier looper";
                type = lib.types.nullOr lib.types.bool;
              };
              period = lib.mkOption {
                default = null;
                description = "period of the notifier looper in seconds";
                type = lib.types.nullOr lib.types.number;
              };
              phase = lib.mkOption {
                default = null;
                description = "phase of the notifier looper in seconds";
                type = lib.types.nullOr lib.types.number;
              };
            };
          };
        };
        switch = lib.mkOption {
          default = null;
          description = "Name of the necrork switch";
          type = lib.types.nullOr lib.types.str;
        };
        timeout = lib.mkOption {
          default = null;
          description = "How long after last hearing from this switch, nodes should consider it dead";
          type = lib.types.nullOr lib.types.ints.u32;
        };
        url = lib.mkOption {
          default = null;
          description = "Base url of the necrork server";
          type = lib.types.nullOr lib.types.str;
        };
      };
    };
  };
  port = lib.mkOption {
    default = null;
    description = "port to serve requests on";
    type = lib.types.nullOr lib.types.int;
  };
  signing-key = lib.mkOption {
    default = null;
    description = "signing key file";
    type = lib.types.nullOr lib.types.str;
  };
}
