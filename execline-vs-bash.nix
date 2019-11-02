let
  inherit (import ./imports.nix)
    pkgs vuizvui nixperiments dhall-prelude runCommandLocal getBins;

  inherit (vuizvui.pkgs.profpatsch)
    runExeclineLocal;

  markdown-to-html = string:
    let
      exe = pkgs.writers.writeHaskell "markdown-to-html" {
        libraries = [
          pkgs.haskellPackages.cmark-gfm
        ];
      } (builtins.readFile ./markdown-to-html/Main.hs);
    in builtins.readFile
      (runExeclineLocal "markdownToHtml" { stdin = string; } [
        "importas" "-iu" "out" "out"
        "redirfd" "-w" "1" "$out"
        exe
      ]);

  execline-vs-bash = import ./notes/execline { inherit pkgs dhall-prelude markdown-to-html runCommandLocal; };

in execline-vs-bash
