# Bevel: Synchronised command line history in an SQLite database

Under construction.

## How it works

* All terminal command history is saved using the `bevel` harness and saved in a local SQLite database.

## Harness

See ./bevel-gather

## Usage

You can use the database however you like, but `bevel` includes some nice general tools:

### Directory switching

Using the `cd` command, you can switch to a previously visited directory quickly:

```
cd $(bevel cd)
```

TODO nice screencast

## Synchronisation

TODO

Run `bevel sync`

## Installation

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

   See `bevel-gather/default.nix`.

2. Install the harness.
   See `bevel-harness`.

3. Make sure you have Haskell's [stack](https://docs.haskellstack.org/en/stable/README/).

   
4. Install the `bevel` CLI.

   ``` shell
   stack install bevel-cli
   ```

4. Optional: Install the server: `stack install bevel-api-server`.

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
