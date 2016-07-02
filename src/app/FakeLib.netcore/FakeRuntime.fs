module Fake.FakeRuntime

open System
open System.IO
open Fake

(* Runtime will restore packages before running the script

A script must start with

(* -- Fake Dependencies 1.0
source http://nuget.org/api/v2

nuget Fake.Travis
nuget Fake.MsBuild
nuget FSharp.Formatting ~> 2.14
-- Fake Dependencies -- *)
#load "./.fake/build.fsx/loadDependencies.fsx"


This way the file can still be edited in editors (after restoring packages initially)
*)
type RawFakeSection =
  { Version : Version
    Section : string }

let readFakeSection (scriptText:string) =
  let startString = "(* -- Fake Dependencies "
  let endString = "-- Fake Dependencies -- *)"
  let start = scriptText.IndexOf(startString) + startString.Length
  let endIndex = scriptText.IndexOf(endString) - 1
  if (start > endIndex) then
    None
  else
    let fakeSectionWithVersion = scriptText.Substring(start, endIndex)
    let newLine = fakeSectionWithVersion.IndexOf("\n")
    let version = fakeSectionWithVersion.Substring(0, newLine)
    let fakeSection = fakeSectionWithVersion.Substring(newLine).Trim()
    Some { Version = Version.Parse(version); Section = fakeSection}

type FakeSection =
 | PaketDependencies of Paket.Dependencies

let parseVersion scriptCacheDir (f : RawFakeSection) =
  match f.Version.Major with
  | 1 -> // Paket
    match f.Version.Minor with
    | 0 -> // In-Line paket.dependencies
      let dependenciesFile = Path.Combine(scriptCacheDir, "paket.dependencies")
      File.WriteAllText(dependenciesFile, f.Section)
      PaketDependencies (Paket.Dependencies(dependenciesFile))
    | 1    // Reference group in existing dependencies file
    | _ -> failwithf "unknown dependencies version %O" f.Version 
  | _ -> failwithf "unknown dependencies version %O" f.Version 

let restoreDependencies cacheDir section =
  Trace.traceFAKE "Restoring is currently not implemented"
  let loadFile = Path.Combine (cacheDir, "loadDependencies.fsx")
  File.WriteAllText(loadFile, "printfn \"loading dependencies... \"")
  ()


let prepareFakeScript printDetails script =
  // read dependencies from the top
  let scriptText = File.ReadAllText(script)
  let section = readFakeSection scriptText
  match section with
  | Some s ->
    let scriptDir = Path.GetDirectoryName (script)
    let cacheDir = Path.Combine(scriptDir, ".fake", Path.GetFileName(script))
    Directory.CreateDirectory (cacheDir) |> ignore
    let dependencies = parseVersion cacheDir s
    restoreDependencies cacheDir dependencies
  | None ->
    Trace.traceFAKE "No dependencies section found in script: %s" script

let prepareAndRunScript printDetails (Fsi.FsiArgs(fsiOptions, scriptPath, scriptArgs) as fsiArgs) envVars useCache =
  prepareFakeScript printDetails scriptPath
  Fsi.runBuildScriptWithFsiArgsAt printDetails fsiArgs envVars useCache