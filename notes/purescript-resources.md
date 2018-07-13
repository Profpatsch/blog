---
title: Purescript Resources
date: 2018-07-13
author: Profpatsch
---

# Purescript Resources

## General

* [Homepage](http://www.purescript.org/)
* [Try Purescript!](http://try.purescript.org/)
* [Good Book: Purescript by Example](https://leanpub.com/purescript/read), by the author of Purescript, Phil Freeman
* [Differences from Haskell](https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md)
* [Types](https://github.com/purescript/documentation/blob/master/language/Types.md)
* [Language Reference](https://github.com/purescript/documentation/tree/master/language)
* [General Documentation](https://github.com/purescript/documentation)
* [Package Documentation](http://pursuit.purescript.org/)
* [Video series](https://www.youtube.com/watch?v=UaXxnWfStbY) by Phil Freeman, converting a medium-sized JS project to Purescript

## Setup

You need the following tools to be productive:

* [The Purescript compiler](http://purescript.org/), called `purs`
* [`nodejs`](https://nodejs.org/en/), for evaluating JS outside of a browser (e.g. in the repl)
* [`npm`](http://npmjs.com), to download JS libraries (should come with `nodejs`)
* [`bower`](https://bower.io/), to download Purescript libraries
* ([`pulp`](https://github.com/purescript-contrib/pulp), a build tool for Purescript projects)

If you have the [`nix`](https://nixos.org/nix/) package manager, you can open a shell with all those dependencies like this:

`nix-shell -p purescript nodejs nodePackages.bower nodePackages.pulp`

It is also possible to download everything through `npm` (even a precompiled x86 purescript binary):

`npm install purescript bower pulp`

The resulting executables can be found in `./node_modules/.bin`.


Once you have the tools installed, you can open a simple repl by first getting the necessary purescript dependencies

`bower install purescript-prelude purescript-psci-support`

and then invoking the compiler like so:

`purs repl 'bower_components/purescript-*/**/*.purs'`

Afterwards you might want to use `pulp` to set up a concrete project. Have fun!

## Tidbits

* [Effects](https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect)
* [Laziness](https://github.com/purescript/purescript-control/blob/v4.0.0/src/Control/Lazy.purs#L10-L11)
* `.` is `<<<` and [comes from `Semigroupoid`!](https://pursuit.purescript.org/packages/purescript-prelude/4.0.1/docs/Control.Semigroupoid)
* [Implementation of a basic module](https://github.com/purescript/purescript-prelude/blob/v4.0.1/src/Data/Eq.purs), `Data.Eq`, and [its base Javascript implementation](https://github.com/purescript/purescript-prelude/blob/v4.0.1/src/Data/Eq.js)

## Interesting libraries

* [`purescript-prelude`](https://pursuit.purescript.org/packages/purescript-prelude), the base typeclasses and instances for builtin types
* Note: Every type that is not a builtin exists in its own package (`purescript-maybe`, `purescript-either`,`purescript-tuples`, …)
* [`purescript-foreign`](https://pursuit.purescript.org/packages/purescript-foreign), for working with plain Javascript values
* [`purescript-argonaut`](https://pursuit.purescript.org/packages/purescript-argonaut), a JSON library
* [`purescript-aff`](https://pursuit.purescript.org/packages/purescript-aff), a library for escaping callback hell
* [`purescript-behaviours`](https://pursuit.purescript.org/packages/purescript-behaviors), an implementation of FRP