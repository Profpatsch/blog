{ system, pkgs, lib }:

let
  dhallPkg = pkgs.dhall;

  # “Pipe” mechanism for nix store files.
  # The given file is piped through the command and put into $out.
  # Can be chained quite easily.
  pipe =
    { name       # of the derivation
    , invocation # list of executable and args
    , file       # file to pipe in
    }:
    assert (builtins.isList invocation && invocation != []);
    let command = builtins.head invocation;
        args = builtins.tail invocation;
    in derivation {
      builder = pkgs.writeScript "toStdin-builder.sh" ''
        #!${pkgs.dash}/bin/dash
        <"$file" $command "$@" > $out
      '';
      inherit command args file name system;
      outputs = [ "out" ];
    };

  # Pipe a file to the dhall language compiler, save result to $out.
  pipeDhall =
    { name            # of the derivation
    , file            # file to pipe in
    , explain ? false # whether errors should be longform
    }:
    pipe {
      inherit name file;
      invocation = [ "${dhallPkg}/bin/dhall" ]
                   ++ lib.optional explain "--explain";
    };

  # Save a nix value in a formatted dhall file.
  # Uses `toDhallValue`.
  toDhallDrv = name: val:
    pipe {
      inherit name;
      invocation = ["${dhallPkg}/bin/dhall-format"];
      file = (pkgs.writeText "${name}-unformatted"
               (toDhallValue {} val).rep);
    };

  # Convert a nix value (that doesn’t contain nulls or functions)
  # into an unformatted dhall value string (no type annotations).
  toDhallValue = {}: v: with builtins;
    if      isInt      v then dhall.unsafe.integer v
    else if isBool     v then dhall.unsafe.bool v
    else if isString   v then dhall.unsafe.text v
    else if isList     v then dhall.unsafe.list (map (toDhallValue {}) v)
    else if isAttrs    v then dhall.unsafe.record
                                (lib.mapAttrs (_: val: toDhallValue {} val) v)
    # nice?
    else if null ==    v then abort "toDhallValue: null can’t be converted"
    else if isFunction v then abort "toDhallValue: functions can’t be converted"
    else abort "toDhallValue: should not happen";

  # Unsafe builders of dhall values.
  # Contain their structured form in `.val`
  # and their formatted dhall string representations in `.rep`.
  # Attention: Recursive values (e.g. list elements)
  # must already be structured dhall values!
  dhall.unsafe = with builtins;
    let val = v: r: { val = v; rep = r; };
    in {
      bool     = b: val b (if b then "True" else "False");
      natural  = n: val n "+${toString n}";
      integer  = i: val i (toString i);
      # double   =
      # TODO: escaping
      text     = t: val t t;
      list     = l: val l "[${lib.concatMapStringsSep ", " (x: x.rep) l}]";
      optional = o: val o (if o == null
                            then "[]"
                            else "[${o.rep}]");
      record   = r: val r ("{ "
                  + lib.concatStringsSep ", " (lib.mapAttrsToList
                                      # TODO: escaping
                        (name: value: "${toString name} = ${value.rep}") r)
                  + " }");
      # union    =
    };


in { inherit pipeDhall toDhallDrv; }
