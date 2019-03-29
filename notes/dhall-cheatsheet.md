---
title: Cheatsheet of the dhall configuration language
date: 2019-03-29
author: Profpatsch
---

This document describes dhall as of version `6.0.0` of the language
standard (dhall-haskell `0.21`).

Pipe file to `dhall` to typecheck/evaluate.  
`dhall --explain` for extensive error explanations.

# Syntax

* `value : type`
* here we use `,` to separate multiple values, which is not valid dhall
* `-- this is a dhall comment`
* `{- this is a multiline comment -}`
* let-syntax
  
  ```
  let
    a = …
  let
    b = … 
  in …
  ```

  (also for function aliases and import names)
  
## Scalars

```
True, False : Bool
0, 1, 2, 3 : Natural
-2, -1, +0, +1, +2 : Integer
-2.0, 3.13158 : Double
"hello world" : Text
```

* TODO: builtin scalar operations

### Text

1. normal double-quoted strings, Haskell style escaping (`"\"abc\"\ndef"`)
2. nix-style interpolated, multi-line strings
    * newline stripping

      ```
      ''
        foo
        bar
      ''
      ```

      is `"\nfoo\nbar\n"`

    * no newlines & escaping `''`

      ```
        ''foo
      '''x
      bar''
      ```

      is `"foo\n''x\nbar"`

    * interpolation

      ```
      let template = 
            \(name : Text) ->
            \(age : Natural) ->
              ''${name} is ${Natural/show age} years old''
      in template "bilbo" 24
      ```

      outputs `"bilbo is 24 years old"`


## Complex Types

### List

`[1,2,3] : List Natural`

* empty lists must be annotated `[] : List Natural`
* items have the same type <s>`[1, "Text"]`</s> (otherwise use Unions)
* `#` concatenates lists: `[ 1 2 3 ] # [ 4 5 ]` => `[ 1 2 3 4 5 ]`

### Optional

* `Some "element" : Optional Text`
* `None Text : Optional Text`

* an optional element
* `None` takes a type, it is `∀(t: Type) -> Optional t`
  * easy to confuse: <s>`None : Natural`</s>, but `None Natural` (application)

### Record

```
{ foo = True
, bar = 2 }
: { foo : Bool
  , bar : Integer }
```

* empty record: `{=} : {}`
* access with `.`: `{ a = False }.a == False`
* record merges
  * `//`: right-leaning value record merge:  
    `{ foo = 4, baz = 7 } // { foo = 5, bar = 6 }`  
    => `{ foo = 5, bar = 6, baz = 7 }`
  * `/\`: recursive value record merge, error on field collision  
    `{ foo = { a = {=}, b = 2 } } /\ { bar = 3, foo = { c = 4 } }`  
    => `{ foo = { a = {=}, b = 2, c = 4 }, bar = 3 }`
  * `//\\`: *type* record merge, analogous to `/\`  
    `{ a : Text, b : Integer } //\\ { c : { d : Natural } }`  
    => `{ a : Text, b : Integer, c : { d : Natural } }`
* record projection
  * `{ a = 1, b = 2, c = 3 }.{ a, b }`  
    => `{ a = 1, b = 2 }`

### Union

TODO: empty fields for 7.0.0

```
let MyUnion = < A : Text | B : Natural | Foo : Text >
let b = MyUnion.B 42
let _ = MyUnion.Foo "hello"
let handlers =
    { A   = \(_ : Text)    -> False
    , B   = \(n : Natural) -> Natural/even n
    , Foo = \(_ : Text)    -> True && False }
in
  (merge handlers b : Bool)
    == True
```

* tagged unions
* merge
  * builtin that matches on the tag
  * needs to produce the same type for each tag
  * always requires an annotation of the output type
  
# Computing

## Functions

```
let f = \(firstArgument : Text) ->
        \(secondArgument : Integer) ->
        "some text ${firstArgument} and int ${Integer/show secondArgument}"
in f "my text" +5
```

* types of input arguments are required (not inferred)

## Guarantees of Dhall Evaluation

* non-turing-complete: all evaluations of well-typed dhall code terminate (eventually)
* total: nothing well-typed will ever crash or throw exceptions
* everything will be evaluated as much as possible
  * `\(x : t) -> +10 * +10` becomes `\(x : t) -> +100`

## Polymorphism

```
let
  const = \(t1 : Type) ->
          \(x : t1) ->
          \(t2 : Type) ->
          \(_ : t2) ->
            x
in let
  id = \(t : Type) ->
       \(x : t) ->
         x
in 
  const Bool False Text "text"
    == id Bool False
```

* specification of type variables happens explicitely as arguments

## Imports

* Imports: Paths are substituted by their contents
  * `./func True 103`
  * ` { foo = "abc", bar = 1 } : https://url/to/type` even

TODO: alternative (fallback) imports with `?`, env imports, `as Text`
imports, `as JSON`? imports, quoted paths, headers, import integrity
hashes, freezing,

# Misc

* Alternative Unicode-Syntax for Functions: `λ(x : t) → …`
* Prelude at https://prelude.dhall-lang.org
