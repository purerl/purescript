[![PureScript](logo.png)](http://purescript.org) - Erlang backend

A small strongly typed programming language with expressive types that compiles to JavaScript, written in and inspired by Haskell.

This fork is a PureScript backend targetting Erlang source. The [purerl](https://github.com/purerl) organisation hosts ports of some core libraries.

To use it, it is recommended to use [psc-package](https://github.com/purescript/psc-package) and the purerl package sets. In a psc-package project, compile with something like
```
psc-package sources | xargs purs 'src/**/*.purs'
```
or if using bower:
```
purs 'bower_components/purescript-*/src/**/*.purs' 'src/**/*.purs'
```

Then build and run the Erlang output:
```
erlc -o ebin/ output/*/*.erl
erl -pa ebin -noshell -eval '(main@ps:main@c())()' -eval 'init:stop()'
```

See [hello-world](https://github.com/purerl/hello-world/) example.

Erlang/OTP 19 supported, subtle & catastrophic bugs have been observed with earlier versions. If you do try with an earlier version minimum 17 is suggested due to character encoding.

# Output

Module names `Foo.Bar` are transformed to a lower-snake cased form `foo_bar` (any non-initial uppercase chars will be preserved as such), with a suffix `@ps` to avoid clashing with built-in erlang modules.

Top level declarations are uniformly output as nullary functions. Identifiers are preserved, with quoting if required. Thus a normal invocation of the output will look like `(main@ps:main())()`.

# Types

| PureScript type | Erlang type | Notes |
| --- | --- | --- |
| `Int` | `integer()` | Arbitrary precision - no longer a `Bounded` |
| `Number` | `float()` |
| `Boolean` | `boolean()` |
| `String` | `string()` | Should consider `binary()` representation |
| `Array` | `array()` | Not to be confused with erlang `[]` list syntax. |
| Records | `#{atom() => any()}` | Map keyed by atoms |
| Tagged union | Tuple with tag element | e.g. `Some 42` is `{some, 42}` |
| Newtype | as underlying type |
| Functions | Function (arity 1 - but see FFI) |
| `Data.Function.Uncurried.FnX` | Function (arity `X`) | Actual higher arity functions - for 'uncurried' functions from tuples see `Erl.Data.Tuple`  | 
| `Erl.Data.List`  | `list()`| Native lists via  `purescript-erl-lists` |
| `Erl.Data.Tuple` | `tuple()` | Native tuples via `purescript-erl-tuples` |

# FFI
In place of `.js` FFI files, the Erlang backend has `.erl` FFI files. As per the regular compiler since 0.9, these must be placed along the corresponding `.purs` file with the same name.


Module name: `Foo.MyModule`
PureScript file `Foo/MyModule.purs`
Erlang file: `Foo/MyModule.erl`
Erlang module: `foo_myModule@foreign`

Note that the FFI code for a module must not only be in a file named correctly, but the module must be named the same as the output module with `@foreign` appended.

FFI files *MUST* export explicitly the exact set of identifiers which will be imported by the corresponding PureScript file. The compiler will check these exports and use them to inform codegen.

*Auto-currying*: functions can be defined with any arity. According to the arity of the export (parsed from the export list) the compiler will automatically apply to the right number of arguments. By extension, values are exported as a function of arity 0 returning that value.

An example:

```purescript
module Foo.Bar where

foreign import f :: Int -> Int -> Int -> Int
```

```erlang
-module(foo_bar@foreign).
-export([f/3]).

f(X, Y, Z) -> X + Y * Z.
```

This could also have been defined as
```erlang
-module(foo_bar@foreign).
-export([f/1]).

f(X) ->
  fun (Y) ->
    fun (Z) ->
      X + Y * Z
    end
  end.
```
