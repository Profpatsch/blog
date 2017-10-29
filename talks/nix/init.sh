#!/bin/sh

if [ -z "$NIXPKGS" ]; then
    export NIXPKGS='<nixpkgs>'
fi

ln -s $(nix-build --no-out-link -A revealjs "$NIXPKGS") reveal.js
