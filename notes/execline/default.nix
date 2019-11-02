{ pkgs, dhall-prelude, markdown-to-html, runCommandLocal }:

let
  lib = pkgs.lib;

  dhallToNix = { name, projectDir, imports, file }:
    let drv = runCommandLocal name {} ''
      cp -r ${projectDir}/* .
      mkdir ./imports
      # TODO: lint for existing imports
      ${lib.concatStrings
        (lib.mapAttrsToList
          (name: dir: "cp -r ${dir} ./imports/${name}\n")
          imports
          )}
      ${pkgs.dhall-nix}/bin/dhall-to-nix < "${file}" > $out
    '';
    in
      import "${drv}";

  oneFile = name: file: runCommandLocal name {} ''
    mkdir $out
    cp ${file} $out/${name}
  '';

  execline-vs-bash = dhallToNix {
    name = "foo";
    projectDir = oneFile "execline-vs-bash.dhall" ./execline-vs-bash.dhall;
    file = "execline-vs-bash.dhall";
    imports = {
      Prelude = dhall-prelude;
    };
  } markdown-to-html;


in  lib.concatMapStringsSep "\n" (v: v { Html = lib.id; Markdown = markdown-to-html; }) execline-vs-bash
