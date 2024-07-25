{ lib }:
{
  database = lib.mkOption {
    default = null;
    description = "database file";
    type = lib.types.nullOr lib.types.str;
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
