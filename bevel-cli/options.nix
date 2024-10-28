{ lib }:
{
  database = lib.mkOption {
    default = null;
    description = "path to the database file";
    type = lib.types.nullOr lib.types.str;
  };
  log-level = lib.mkOption {
    default = null;
    description = "minimal severity of log messages";
    type = lib.types.nullOr (lib.types.enum [
      "Debug"
      "Info"
      "Warn"
      "Error"
    ]);
  };
  max-options = lib.mkOption {
    default = null;
    description = "maximum number of options to show when selecting";
    type = lib.types.nullOr lib.types.ints.u8;
  };
  password = lib.mkOption {
    default = null;
    description = "password";
    type = lib.types.nullOr lib.types.str;
  };
  password-file = lib.mkOption {
    default = null;
    description = "password";
    type = lib.types.nullOr lib.types.str;
  };
  server-url = lib.mkOption {
    default = null;
    description = "server base url";
    type = lib.types.nullOr lib.types.str;
  };
  username = lib.mkOption {
    default = null;
    description = "user name";
    type = lib.types.nullOr lib.types.str;
  };
}
