let
  inherit (import ./imports.nix)
    pkgs vuizvui;
  inherit (vuizvui.pkgs.profpatsch)
    writeExecline;

  home-dir =
    # avoid matching this file
    let home = "/" + "home/";
    in writeExecline "check-home-dir" {} [
      "if" [ "${pkgs.git}/bin/git" "grep" home ]
      "foreground" [ "echo" "found some files referencing /home!" ]
      "exit" "1"
    ];

in {
   inherit home-dir;
}
