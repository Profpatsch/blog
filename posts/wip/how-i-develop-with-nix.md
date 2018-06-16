# How I develop with nix

## Tenets

* Avoid state at all cost, centralize it if it is necessary
  * Don’t use `nix-env`
  * Everything comes from one source
  * Generated files are never checked in
* Everything must work locally
  * Everything is tested as local as possible
    * Upstream (unit) tests in `checkPhase` of build
    * Derivation tests as simple package tests
    * Service (daemon, network) interaction in VM tests

## Nix proper

* Difference between `1.12` and `2.0` utils, which are not yet stable
  * `1.12` utils use `-`: `nix-build`, `nix-instantiate`, `nix-store`, …
  * `2.0` has a `nix` utility, with `nix build`, `nix eval`, …
    * those have completely different semantics
  * I still use the `1.12` utils, but there is a section on the new utils
  * ATTENTION: man pages link to the old utils
    * Even `man nix build`, since `man` has a feature that redirects one subcommand to the hyphenated version, `nix-build` in this case 
* `NIX_PATH`
  * TODO
* `-I` is used as a CLI argument that overrides parts of `NIX_PATH`
  * Not for every tool, I mention when it works (consult man pages)
  * To override e.g. the `<nixpkgs>` used, use `-I nixpkgs=/path/to/nixpkgs`
  * ATTENTION: *needs* a space after the `-I`, will be ignored otherwise
    * Yes, this is shit. But so is most of the old utils CLIs

### Don’t use `nix-env`

Seriously, don’t use it. The tool introduces state and will fix “installed” packages to versions that don’t get updated with the rest of your system. This goes contrary to why you’d want to use nix/NixOS in the first place. You will have a sorry time and have been warned.

Instead, use the techniqes I am going to describe here.

### `nix-shell`

Where to start. This tool is probably the most helpful thing I’ve ever used anywhere. Even though it’s just a very sad perl script around `nix-instantiate` and `nix-store` with horrible CLI and horribly leaky semantics.

#### `nix-shell` 101

TODO

* `--pure`
* `-p` for packages (or something that evaluates to a package, with `<nixpkgs>` in scope)
  * Alias `nsp` to `nix-shell -p`
  * You need `-I` if you don’t want the `NIX_PATH` nixpkgs
* `-A` for the builder env of a package (don’t confuse with `nix-build`’s `-A`)

#### `nix-shell` Shebangs

Start your scripts with

```
#!/usr/bin/env nix-shell
#!nix-shell -i interpreter -p package1 package2 …
```

if they can assume `nix` to be installed. Necessary packages will be installed when you run the script. This means one `nix-instantiate` per invocation, so startup time is not great. But for ad-hoc scripts (e.g. in `~/scripts`) or seldomly run scripts (like a manual backup) it works great. The interpreter needs to be in scope, so add it to your packages.


## NixOS

### Machine management

Machine configs are inside a repository which is laid around `nixpkgs` and extends it with a few packages, modules and a bit of hydra setup (most of this work was done by @aszlig).
The repository is called [vuizvui](https://headcounter.org/hydra/jobset/openlab/vuizvui).

Machines get their own attributes (e.g. `(import <vuizvui/machines>).profpatsch.katara` for my main development laptop), with their evaluated NixOS configurations at `machine.config`.

### Setting up `NIX_PATH` and channels

On development and manually managed machines local checkout of `nixpkgs` and `NIX_PATH` points to it:

```nix
{
  nix.nixPath = [
    "nixpkgs=${myLib.philip.home}/nixpkgs"
    "nixos-config=${pkgs.writeText "katara-configuration.nix" ''
      (import <vuizvui/machines>).profpatsch.katara.config
    ''}"
  ];
}
```

This means every nix command uses the code of the current checkout in `$HOME/nixpkgs`, everything comes from one central place that is managed with git commands. (The exception is `nix-build`, which uses the current or an explicitely specified directory.)

### Building machines on hydra

TODO