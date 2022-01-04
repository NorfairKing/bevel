{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
}:
let
  bevel-production = import (./nixos-module.nix) { envname = "production"; bevelPackages = pkgs.bevelPackages; };
  home-manager = import (sources.home-manager + "/nixos/default.nix");
  port = 8001;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "bevel-module-test";
    nodes = {
      server = {
        imports = [
          bevel-production
        ];
        services.bevel.production = {
          enable = true;
          api-server = {
            enable = true;
            inherit port;
          };
        };
      };
      client = {
        imports = [
          home-manager
        ];
        users.users.testuser.isNormalUser = true;
        home-manager = {
          useGlobalPkgs = true;
          users.testuser = { pkgs, ... }: {
            imports = [
              ./home-manager-module.nix
            ];
            xdg.enable = true;
            home.stateVersion = "20.09";
            programs.bevel = {
              enable = true;
              bevelPackages = pkgs.bevelPackages;
              sync = {
                enable = true;
                server-url = "server:${builtins.toString port}";
                username = "testuser";
                password = "testpassword";
              };
            };
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      server.start()
      client.start()
      server.wait_for_unit("multi-user.target")
      client.wait_for_unit("multi-user.target")

      server.wait_for_open_port(${builtins.toString port})
      client.succeed("curl server:${builtins.toString port}")

      client.wait_for_unit("home-manager-testuser.service")


      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      client.succeed(su("testuser", "cat ~/.config/bevel/config.yaml"))

      client.succeed(su("testuser", "bevel register"))
      client.succeed(su("testuser", "bevel login"))
      client.succeed(su("testuser", "bevel sync"))
    '';
  }
)
