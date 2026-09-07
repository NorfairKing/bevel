{ lib }:
{
  database = lib.mkOption {
    default = null;
    description = "database file";
    type = lib.types.nullOr lib.types.str;
  };
  download-batch-size = lib.mkOption {
    default = null;
    description = "maximum number of commands to send in one download response";
    type = lib.types.nullOr lib.types.int;
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
