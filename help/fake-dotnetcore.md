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

- The migration path will be that "FakeLib" is updated with the new dotnetcore compatible API.
  The old API will be marked as obsolete. 
  In theory you can fix all warnings and switch to dotnetcore by inserting the new header.

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

- See https://github.com/matthid/FAKE/blob/coreclr/dncbuild.fsx
  Note that with the "new" API you should call the modules directly instead of opening them. 
  Therefore this example is actually pretty bad because it just opened everything (for minimal diff to the "normal" build.fsx)

TBD.


## Downloads

Currently Releases are on my branch: https://github.com/matthid/FAKE/releases/
Alpha4 is the last known working version: https://github.com/matthid/FAKE/releases/tag/core-v1.0-alpha-04