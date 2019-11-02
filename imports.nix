let
  vuizvui = import (pkgs.fetchFromGitHub {
    owner = "openlab-aux";
    repo = "vuizvui";
    rev = "d9addb8fb3f60ce524c482f3993f04f5114fc41f";
    sha256 = "1l0pjk88vrv6igjwyghg121w3ysvpsq61pr8l9gjaafgqxs56gs0";
  }) { };

  dhall-simple = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-dhall-nix";
    rev = "01844fa08e20fa4460741f40175857d6fd15ae8e";
    sha256 = "07zr7y93hmn6gxqcsjc1db7fc6fhnlc0jwv2snzqsk812ypacbs6";
  }) {};

  # TODO: sticky pkgs
  pkgs = import <nixpkgs> {
    overlays = [ (_: _: {
      dhall = dhall-simple.dhall-simple;
      dhall-nix = dhall-simple.dhall-nix-simple;
      dhall-json = dhall-simple.dhall-json-simple;
    }) ];
  };

  nixperiments = import (pkgs.fetchFromGitHub {
    owner = "Profpatsch";
    repo = "nixperiments";
    rev = "adec5d3458dc24a062f2582531208241b2aa61b4";
    sha256 = "07zr7y93hmn6gxqcsjc1db7fc6fhnlc0jwv2s5zqsk812ypacbs6";
  }) { nixpkgs = pkgs; };

  dhall-prelude = "${pkgs.fetchFromGitHub {
    owner = "dhall-lang";
    repo = "dhall-lang";
    rev = "084ec169695c09e8ac92c4c0dfeb4f9b8188b593";
    sha256 = "12x4qkhx15cj6zq6m7v943blnncaqh8bsq4i3276l2zbngv499rd";
  }}/Prelude";

  runCommandLocal = name: args: cmd:
    pkgs.runCommand name (args // {
      preferLocalBuild = true;
      allowSubstitutes = false;
    }) cmd;


in {
  inherit pkgs vuizvui nixperiments
    dhall-simple dhall-prelude
    runCommandLocal;
}
