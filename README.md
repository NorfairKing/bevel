# Bevel: Synchronised command line history in an sqlite database

Under construction.

## How it works

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

### Without Nix

1. Compile the gather tool. See `bevel-gather`.
2. Install the harness. See `bevel-harness`.
3. Install the CLI. `stack install bevel-cli`.
4. Optional: Install the server: `stack install bevel-api-server`.

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
