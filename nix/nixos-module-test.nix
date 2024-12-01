{ nixosTest
, home-manager
, bevel-nixos-module-factory
, bevel-home-manager-module
}:
let
  bevel-production = bevel-nixos-module-factory { envname = "production"; };
  port = 8001;
in
nixosTest {
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
          openFirewall = true;
          config = {
            inherit port;
          };
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
            bevel-home-manager-module
          ];
          home.stateVersion = "24.11";
          home.packages = with pkgs; [
            sqlite
          ];
          programs.bash.enable = true;
          programs.zsh.enable = true;
          xdg.enable = true;
          programs.bevel = {
            enable = true;
            harness = {
              bash = {
                enable = true;
                bindings = true;
              };
              zsh = {
                enable = true;
                bindings = true;
              };
            };
            sync.enable = true;
            sync.autosync = true;
            config = {
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
    server.wait_for_open_port(${builtins.toString port})

    client.wait_for_unit("multi-user.target")
    client.require_unit_state("home-manager-testuser.service", "inactive")
    client.succeed("curl server:${builtins.toString port}")


    def su(user, cmd):
        return f"su - {user} -c {quote(cmd)}"


    client.succeed(su("testuser", "cat ~/.config/bevel/config.yaml"))

    client.succeed(su("testuser", "bevel register"))
    client.succeed(su("testuser", "bevel login"))
    client.succeed(su("testuser", "bevel sync"))
  '';
}
