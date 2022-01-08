# Bevel: Synchronised command line history in an SQLite database

Under construction.

## How it works

* All terminal command history is saved using the `bevel` harness and saved in a local SQLite database.
* Use this history for advanced terminal usage using the `bevel` CLI.
* Synchronise your command history across devices using `bevel sync`.

## Usage

You can use the database however you like, but `bevel` includes some nice general tools:

### Directory switching

Using the `cd` command, you can switch to a previously visited directory quickly:

```
cd $(bevel cd) # Or use the C-p binding
```

TODO nice screencast

### Command repetition

Bevel offers a replacement for the `C-r` functionality of your shell.

```
$(bevel repeat) # Or use the C-r binding
```

TODO nice screencast

### Building from source, without Nix

0. Get yourself a copy of the Sqlite source code: `sqlite3.c`
1. Compile the `bevel-gather` utility and put it on your path.

   ``` shell
   cd bevel-gather
   musl-gcc \
     -Wall -Wextra -pedantic -O2 -s -static -Wl,--gc-sections -Wl,--strip-all \
     bevel-gather.c \
     sqlite3.c \
     -o $out/bin/bevel-gather
   ```

2. Install the harness.

   For `bash`, add this line to the end of your `~/.bashrc`:

   ```
   source /path/to/bevel/bevel-harness/harness.bash
   ```

   For `zsh`, add this line to the end of your `~/.zshrc`:

   ```
   source /path/to/bevel/bevel-harness/harness.zsh
   ```

3. Make sure you have Haskell's [stack](https://docs.haskellstack.org/en/stable/README/).

   
4. Install the `bevel` CLI.

   ``` shell
   stack install bevel-cli
   ```

5. Install the bindings.

   For `bash`, add this line to the end of your `~/.bashrc`:

   ```
   source /path/to/bevel/bevel-harness/bindings.bash
   ```

   For `zsh`, add this line to the end of your `~/.zshrc`:

   ```
   source /path/to/bevel/bevel-harness/bindings.zsh
   ```

6. Optional: Install the server: `stack install bevel-api-server`.

   ``` shell
   stack install bevel-api-server
   ```

### With Nix

#### Home manager integration

``` nix
{ pkgs, lib, ... }:
with lib;
let
  bevelModule =
    builtins.fetchGit {
      url = "https://github.com/NorfairKing/bevel";
      rev = "0000000000000000000000000000000000000000"; # Put a recent commit hash here.
      ref = "master";
    } + "/nix/home-manager-module.nix";
in
{
  imports = [
    bevelModule
    # [...]
  ];
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
  };
}
```

See `nix/home-manager-module.nix`.


#### NixOS integration to run the server

``` nix
{ lib, pkgs, config, ... }:
let
  bevel-production = (
    import (
      builtins.fetchGit {
        url = "https://github.com/NorfairKing/bevel";
        rev = "0000000000000000000000000000000000000000"; # Put a recent commit hash here.
        ref = "master";
      } + "/nix/nixos-module.nix"
    ) { envname = "production"; }
  );
in
{
  imports = [
    bevel-production
  ];
  config = {
    services.bevel.production.api-server = {
      enable = true;
      api-server = {
        enable = true;
        log-level = "Warn";
        hosts = [ "api.bevel.mydomain.com" ];
        port = 8402;
        local-backup = {
          enable = true;
        };
      };
    };
  };
}
```

See `nix/nixos-module.nix`.
