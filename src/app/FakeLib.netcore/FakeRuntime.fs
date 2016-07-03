module Fake.FakeRuntime

open System
open System.IO
open Fake

(* Runtime will restore packages before running the script

A script must start with

(* -- Fake Dependencies paket-inline
source http://nuget.org/api/v2

nuget Fake.Travis
nuget Fake.MsBuild
nuget FSharp.Formatting ~> 2.14
-- Fake Dependencies -- *)
#load "./.fake/build.fsx/loadDependencies.fsx"


This way the file can still be edited in editors (after restoring packages initially).
It's possible to use an existing file:

(* -- Fake Dependencies paket.dependencies
file ./paket.dependencies
group Build
-- Fake Dependencies -- *)
#load "./.fake/build.fsx/loadDependencies.fsx"

*)
type RawFakeSection =
  { Header : string
    Section : string }

let readFakeSection (scriptText:string) =
  let startString = "(* -- Fake Dependencies "
  let endString = "-- Fake Dependencies -- *)"
  let start = scriptText.IndexOf(startString) + startString.Length
  let endIndex = scriptText.IndexOf(endString) - 1
  if (start >= endIndex) then
    None
  else
    let fakeSectionWithVersion = scriptText.Substring(start, endIndex - start)
    let newLine = fakeSectionWithVersion.IndexOf("\n")
    let header = fakeSectionWithVersion.Substring(0, newLine).Trim()
    let fakeSection = fakeSectionWithVersion.Substring(newLine).Trim()
    Some { Header = header; Section = fakeSection}

type FakeSection =
 | PaketDependencies of Paket.Dependencies * group : String option

let readAllLines (r : TextReader) =
  seq {
    let mutable line = r.ReadLine()
    while not (isNull line) do
      yield line
      line <- r.ReadLine()
  }

let parseHeader scriptCacheDir (f : RawFakeSection) =
  match f.Header with
  | "paket-inline" ->
    let dependenciesFile = Path.Combine(scriptCacheDir, "paket.dependencies")
    File.WriteAllText(dependenciesFile, f.Section)
    PaketDependencies (Paket.Dependencies(dependenciesFile), None)
  | "paket.depedencies" ->
    let groupStart = "group "
    let fileStart = "file "
    let readLine (l:string) : (string * string) option =
      if l.StartsWith groupStart then ("group", (l.Substring groupStart.Length).Trim()) |> Some
      elif l.StartsWith fileStart then ("file", (l.Substring fileStart.Length).Trim()) |> Some
      elif String.IsNullOrWhiteSpace l then None
      else failwithf "Cannot recognise line in dependency section: '%s'" l
    let options =
      (use r = new StringReader(f.Section)
       readAllLines r |> Seq.toList)
      |> Seq.choose readLine
      |> dict
    let group =
      match options.TryGetValue "group" with
      | true, gr -> Some gr
      | _ -> None
    let file =
      match options.TryGetValue "file" with
      | true, depFile -> depFile
      | _ -> "paket.dependencies"
    PaketDependencies (Paket.Dependencies(file), group)
  | _ -> failwithf "unknown dependencies header '%s'" f.Header 

  (*
    Trace.log "Restoring with paket..."
    // Check if restore is enough
    let lockFilePath = Paket.DependenciesFile.FindLockfile paketDependencies.DependenciesFile
    if File.Exists lockFilePath.FullName then
      // Restore only
      paketDependencies.Restore(false, group, [], false, true)
      |> ignore
    else 
      // Update
      paketDependencies.UpdateGroup(groupStr, false, false, false, false, false, Paket.SemVerUpdateMode.NoRestriction, false)
      |> ignore

    let lockFile = paketDependencies.GetLockFile()
    let groupStr = match group with Some m -> m | None -> "Main"
    let groupName = Paket.Domain.GroupName (groupStr)
    let lockGroup = lockFile.GetGroup groupName
    
    let assemblies = 
    File.WriteAllText(loadFile, "printfn \"loading dependencies... \"")

  *)


let paketCachingProvider (paketDependencies:Paket.Dependencies) group =
  let groupStr = match group with Some g -> g | None -> "Main"
  let groupName = Paket.Domain.GroupName (groupStr)
  let framework = Paket.FrameworkIdentifier.DotNetStandard (Paket.DotNetStandardVersion.V1_6)
  let lockFilePath = Paket.DependenciesFile.FindLockfile paketDependencies.DependenciesFile
  let restoreOrUpdate () =
    Trace.log "Restoring with paket..."
    if File.Exists lockFilePath.FullName then
      // Restore only
      paketDependencies.Restore(false, group, [], false, true)
      |> ignore
    else 
      // Update
      paketDependencies.UpdateGroup(groupStr, false, false, false, false, false, Paket.SemVerUpdateMode.NoRestriction, false)
      |> ignore
    let lockFile = paketDependencies.GetLockFile()
    let lockGroup = lockFile.GetGroup groupName
    
    lockGroup.Resolution
    |> Seq.map (fun kv -> 
      let packageName = kv.Key
      let package = kv.Value
      package)
    |> Seq.toList
    |> Paket.LoadingScripts.PackageAndAssemblyResolution.getPackageOrderResolvedPackage
    |> Seq.collect (fun p ->
      let installModel = paketDependencies.GetInstalledPackageModel(group, p.Name.ToString())
      Paket.LoadingScripts.PackageAndAssemblyResolution.getDllsWithinPackage framework installModel)
    |> Seq.choose (fun fi ->
      let fullName = fi.FullName
      try let assembly = Mono.Cecil.AssemblyDefinition.ReadAssembly fullName
          { Fake.Fsi.AssemblyInfo.FullName = assembly.Name.FullName
            Fake.Fsi.AssemblyInfo.Version = assembly.Name.Version.ToString()
            Fake.Fsi.AssemblyInfo.Location = fullName } |> Some
      with e -> printfn "Could not load '%s': %O" fullName e; None)
    |> Seq.toList
  // Restore or update immediatly, because or everything might be OK -> cached path.
  let mutable assemblies = restoreOrUpdate()
  { new Fake.Fsi.ICachingProvider with
      member x.Invalidate cachePath =
        Trace.log "Invalidating cache..."
        if File.Exists cachePath then File.Delete cachePath
        // TODO: In future we want to take special care for external dependency files?
        // They should have their own hash in cache
        // BUG: lockfile only needs to be deleted when dependencies file changed, not even when the script changed!
        if File.Exists lockFilePath.FullName then File.Delete lockFilePath.FullName
      member x.MapFsiOptions opts =
          // Restore again -> cache invalid -> dependencies file might have changed.
          assemblies <- restoreOrUpdate()
          let options = Yaaf.FSharp.Scripting.FsiOptions.ofArgs opts
          let references = assemblies |> List.map (fun (a:Fake.Fsi.AssemblyInfo) -> a.Location)
          { options with
              NoFramework = true
              Debug = Some Yaaf.FSharp.Scripting.DebugMode.Portable
              References = references @ options.References }
          |> fun options -> options.AsArgs
      member x.TryLoadCache (cachePath) = Some assemblies
      member x.ResolveAssembly a =
          match a with
          | Choice1Of2 name -> 
            // Todo: resolve from 'assemblies'?
            null
          | Choice2Of2 a -> a
      member x.TrySaveCache (cachePath) = true }

let restoreDependencies cacheDir section =
  let loadFile = Path.Combine (cacheDir, "loadDependencies.fsx")
  match section with
  | PaketDependencies (paketDependencies, group) ->
    paketCachingProvider paketDependencies group
    

let prepareFakeScript printDetails script =
  // read dependencies from the top
  let scriptDir = Path.GetDirectoryName (script)
  let cacheDir = Path.Combine(scriptDir, ".fake", Path.GetFileName(script))
  Directory.CreateDirectory (cacheDir) |> ignore
  let scriptText = File.ReadAllText(script)
  let section = readFakeSection scriptText
  match section with
  | Some s ->
    let section = parseHeader cacheDir s
    restoreDependencies cacheDir section
  | None ->
    Trace.traceFAKE "No dependencies section found in script: %s" script
    Fake.Fsi.Cache.defaultProvider

let prepareAndRunScriptRedirect printDetails (Fsi.FsiArgs(fsiOptions, scriptPath, scriptArgs) as fsiArgs) envVars onErrMsg onOutMsg useCache =
  let provider = prepareFakeScript printDetails scriptPath
  Fsi.runFakeWithCache provider printDetails fsiArgs envVars onErrMsg onOutMsg useCache

let prepareAndRunScript printDetails fsiArgs envVars useCache =
  prepareAndRunScriptRedirect printDetails fsiArgs envVars (Fake.Fsi.onMessage true) (Fake.Fsi.onMessage false) useCache