# Bevel: Synchronised command line history in an sqlite database

Under construction.


## Harness

TODO

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

## Nix

### Home manager integration

See `nix/home-manager-module.nix`.


### NixOS integration

See `nix/nixos-module.nix`.
