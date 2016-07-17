# Fake dotnetcore

## Why?

The goals are:

 - Provide a easy to use cross platform way to use FAKE. With a good bootstrapping experience
 - Cleanup 'FakeLib' 
 - Extract reusable libraries and make them usable for your projects or the fsi!
 - Make it easier to extend FAKE for your own use-case
 - Provide an easy way for simple scripting, automate everything, everywhere.

Please read https://github.com/fsharp/FAKE/issues/1232

## What is the migration path?

TBD. The current idea is:

- The old 'Fake' will be obsolete but still being updated for quite a while
  TBD: There might be a wrapper on top of the new code providing the "old" cli for compatibility

- Fake dotnetcore will have a new set of command line options, therefore you need to update all the places
  where you call Fake in your Build infrastructure.

- The migration path will be a compatible set of reusable libraries which will be marked as obsolete.
  They mimic the old "FakeLib".
  In this process FakeLib will be updated with the "new" Api as well, while marking the "old" Api as obsolete.
  Basically all you need to do is add a header to your build-file to tell Fake about your dependencies and remove `#load "FakeLib.dll"`.

- Next step will be to remove everything you don't need.

- Now update all packages to their latest version and use non-obsolete APIs (and fix potential breaking changes).

## How to specify dependencies?

The Fake runtime will restore packages before running the script. All you need to do is specify them

To tell Fake which dependencies are needed a script should start with

```
(* -- Fake Dependencies ***header***
*** Dependencies ***
-- Fake Dependencies -- *)
#load "./.fake/build.fsx/loadDependencies.fsx"
```

The last line `#load` is not required, however
this way the file can still be edited in editors (after restoring packages initially).
Fake will write a `loadDependencies.fsx` file for you importing all required references.

There are two headers known by Fake:

### paket-inline

This way you can specify all your dependencies via pakets `paket.dependencies` syntax inline in your Fake script.
Fake will implicitly use the "Main" paket group for the script.

```
(* -- Fake Dependencies paket-inline
source http://nuget.org/api/v2

nuget Fake.Travis
nuget Fake.MsBuild
nuget FSharp.Formatting ~> 2.14
-- Fake Dependencies -- *)
#load "./.fake/build.fsx/loadDependencies.fsx"
```

### paket.dependencies

It's also possible to use an existing `paket.dependencies` file and specify the file and group to use (defaults to "paket.dependencies" and "Main"):

```
(* -- Fake Dependencies paket.dependencies
file ./paket.dependencies
group Build
-- Fake Dependencies -- *)
#load "./.fake/build.fsx/loadDependencies.fsx"
```


## Examples

TBD.