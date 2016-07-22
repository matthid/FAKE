/// Contains helper functions which allow to interact with the F# Interactive.
#if CORE_CLR
module Fake.Fsi
open Fake.Environment
open Fake.String
open Fake.Trace
#if NETSTANDARD1_5
open System.Runtime.Loader
#endif
#else
[<AutoOpen>]
module Fake.FSIHelper
#endif

open System
open System.IO
open System.Diagnostics
open System.Threading
open System.Text.RegularExpressions
open System.Xml.Linq
open Yaaf.FSharp.Scripting

#if !CORE_CLR
let private FSIPath = @".\tools\FSharp\;.\lib\FSharp\;[ProgramFilesX86]\Microsoft SDKs\F#\4.0\Framework\v4.0;[ProgramFilesX86]\Microsoft SDKs\F#\3.1\Framework\v4.0;[ProgramFilesX86]\Microsoft SDKs\F#\3.0\Framework\v4.0;[ProgramFiles]\Microsoft F#\v4.0\;[ProgramFilesX86]\Microsoft F#\v4.0\;[ProgramFiles]\FSharp-2.0.0.0\bin\;[ProgramFilesX86]\FSharp-2.0.0.0\bin\;[ProgramFiles]\FSharp-1.9.9.9\bin\;[ProgramFilesX86]\FSharp-1.9.9.9\bin\"
#endif

let createDirectiveRegex id = 
    Regex("^\s*#" + id + "\s*(@\"|\"\"\"|\")(?<path>.+?)(\"\"\"|\")", RegexOptions.Compiled ||| RegexOptions.Multiline)

let loadRegex = createDirectiveRegex "load"
let rAssemblyRegex = createDirectiveRegex "r"
let searchPathRegex = createDirectiveRegex "I"

let private extractDirectives (regex : Regex) scriptContents = 
    regex.Matches scriptContents
    |> Seq.cast<Match>
    |> Seq.map(fun m -> m.Groups.Item("path").Value)
    
type Script = {
    Content : string
    Location : string
    SearchPaths : string seq
    IncludedAssemblies : Lazy<string seq>
}

let getAllScriptContents (pathsAndContents : seq<Script>) = 
    pathsAndContents |> Seq.map(fun s -> s.Content)
let getIncludedAssembly scriptContents = extractDirectives rAssemblyRegex scriptContents
let getSearchPaths scriptContents = extractDirectives searchPathRegex scriptContents

let rec getAllScripts scriptPath : seq<Script> = 
    let scriptContents = File.ReadAllText scriptPath
    let searchPaths = getSearchPaths scriptContents |> Seq.toList

    let loadedContents = 
        extractDirectives loadRegex scriptContents
        |> Seq.collect (fun path -> 
            let path = 
                if Path.IsPathRooted path then
                    path
                else
                    let pathMaybe = 
                        ["./"] @ searchPaths
                        |> List.map(fun searchPath ->
                            if Path.IsPathRooted searchPath then
                                Path.Combine(searchPath, path)
                            else
                                Path.Combine(Path.GetDirectoryName scriptPath, searchPath, path))
                        |> List.tryFind File.Exists

                    match pathMaybe with 
                    | None -> failwithf "Could not find script '%s' in any paths searched. Searched paths:\n%A" path searchPaths
                    | Some x -> x
            getAllScripts path
        )
    let s = 
      { Location = scriptPath
        Content = scriptContents
        SearchPaths = searchPaths
        IncludedAssemblies = lazy(getIncludedAssembly scriptContents) }
    Seq.concat [List.toSeq [s]; loadedContents]

let getStringHash (s:string) =
    use sha256 = System.Security.Cryptography.SHA256.Create()
    s
    |> System.Text.Encoding.UTF8.GetBytes
    |> sha256.ComputeHash
    |> BitConverter.ToString
    |> fun s -> s.Replace("-", "")

let getScriptHash pathsAndContents fsiOptions =
    (getAllScriptContents pathsAndContents |> String.concat "\n")
    + (pathsAndContents |> Seq.map(fun x -> x.Location |> normalizePath) |> String.concat "\n")
    + (fsiOptions |> String.concat "\n")
    |> getStringHash

type AssemblyInfo = {
    FullName : string
    Version : string
    Location : string
}

type ICachingProvider =
    abstract TryLoadCache : configFile:string -> AssemblyInfo list option
    abstract TrySaveCache : configFile:string -> bool
    abstract Invalidate : configFile:string -> unit
    abstract MapFsiOptions : string[] -> string[]
    abstract ResolveAssembly : Choice<System.Reflection.AssemblyName, System.Reflection.Assembly> -> System.Reflection.Assembly

module internal Cache =
    let xname name = XName.Get(name)
    let create (loadedAssemblies : Reflection.Assembly seq) =
        let xelement name = XElement(xname name)
        let xattribute name value = XAttribute(xname name, value)

        let doc = XDocument()
        let root = xelement "FAKECache"
        doc.Add(root)
        let assemblies = xelement "Assemblies"
        root.Add(assemblies)

        let assemNodes =
            loadedAssemblies
            |> Seq.map(fun assem ->
                let ele = xelement "Assembly"
                ele.Add(xattribute "Location" assem.Location)
                ele.Add(xattribute "FullName" assem.FullName)
                ele.Add(xattribute "Version" (assem.GetName().Version.ToString()))
                ele)
            |> Seq.iter(assemblies.Add)
        doc

    let read (path : string) = 
        let doc = XDocument.Load(path)
        //let root = doc.Descendants() |> Seq.exactlyOne
        let assembliesEle = doc.Descendants(xname "Assemblies") |> Seq.exactlyOne
        assembliesEle.Descendants()
        |> Seq.map(fun assemblyEle ->
            let get name = assemblyEle.Attribute(xname name).Value
            { Location = get "Location"
              FullName = get "FullName"
              Version = get "Version" })
        |> Seq.toList

    let defaultProvider =
#if !NETSTANDARD1_6
        { new ICachingProvider with
            member x.MapFsiOptions options = options
            member x.Invalidate cachePath = if File.Exists cachePath then File.Delete cachePath
            member x.TryLoadCache (cachePath) =
                if File.Exists cachePath then
                    Some (read cachePath)
                else None
            //member x.GetAssembliesFromCache c = c.Assemblies
            member x.ResolveAssembly a =
                match a with
                | Choice1Of2 name -> null
                | Choice2Of2 a -> a
            member x.TrySaveCache (cachePath) =
                let dynamicAssemblies =
                    System.AppDomain.CurrentDomain.GetAssemblies()
                    |> Seq.filter(fun assem -> assem.IsDynamic)
                    |> Seq.map(fun assem -> assem.GetName().Name)
                    |> Seq.filter(fun assem -> assem <> "FSI-ASSEMBLY")
                    |> Seq.filter(fun assem -> not <| assem.StartsWith "FAKE_CACHE_")
                    // General Reflection.Emit helper (most likely harmless to ignore)
                    |> Seq.filter(fun assem -> assem <> "Anonymously Hosted DynamicMethods Assembly")
                    // RazorEngine generated
                    |> Seq.filter(fun assem -> assem <> "RazorEngine.Compilation.ImpromptuInterfaceDynamicAssembly")
                    |> Seq.cache
                if dynamicAssemblies |> Seq.length > 0 then
                    let msg =
                        sprintf "Dynamic assemblies were generated during evaluation of script (%s).\nCan not save cache."
                            (System.String.Join(", ", dynamicAssemblies))
                    trace msg
                    false
                else
                    let assemblies =
                        System.AppDomain.CurrentDomain.GetAssemblies()
                        |> Seq.filter(fun assem -> not assem.IsDynamic)
                        |> Seq.filter(fun assem -> not <| assem.GetName().Name.StartsWith "FAKE_CACHE_")
                        // They are not dynamic, but can't be re-used either.
                        |> Seq.filter(fun assem -> not <| assem.GetName().Name.StartsWith("CompiledRazorTemplates.Dynamic.RazorEngine_"))

                    let cacheConfig : XDocument = create assemblies
                    cacheConfig.Save (cachePath)
                    true }
#else
        { new ICachingProvider with
            member x.MapFsiOptions opts =
                let options = FsiOptions.ofArgs opts
                if not options.NoFramework then // Caller should take care!
                    let basePath = System.AppContext.BaseDirectory
                    let references =
                        System.IO.Directory.GetFiles(basePath, "*.dll")
                        |> Seq.filter (fun r -> not (System.IO.Path.GetFileName(r).ToLowerInvariant().StartsWith("api-ms")))
                        |> Seq.filter (fun r ->
                            try Mono.Cecil.ModuleDefinition.ReadModule(r) |> ignore
                                true
                            with e -> false)
                        |> Seq.toList
                    { options with
                        NoFramework = true
                        Debug = Some DebugMode.Portable
                        References = (references) @ options.References }
                else options
                |> (fun options -> options.AsArgs)
            member x.Invalidate cachePath = if File.Exists cachePath then File.Delete cachePath
            member x.TryLoadCache (cachePath) = 
                traceFAKE "Default caching is disabled on dotnetcore, see https://github.com/dotnet/coreclr/issues/919#issuecomment-219212910"
                None
            //member x.GetAssembliesFromCache c = c.Assemblies
            member x.ResolveAssembly a =
                match a with
                | Choice1Of2 name -> null
                | Choice2Of2 a -> a
            member x.TrySaveCache (cachePath) = false }
#endif

#if !CORE_CLR
/// The path to the F# Interactive tool.
let fsiPath =
    let ev = environVar "FSI"
    if not (isNullOrEmpty ev) then ev else
    if isUnix then
        let paths = appSettings "FSIPath" FSIPath
        // The standard name on *nix is "fsharpi"
        match tryFindFile paths "fsharpi" with
        | Some file -> file
        | None -> 
        // The early F# 2.0 name on *nix was "fsi"
        match tryFindFile paths "fsi" with
        | Some file -> file
        | None -> "fsharpi"
    else
        let dir = Path.GetDirectoryName fullAssemblyPath
        let fi = fileInfo (Path.Combine(dir, "fsi.exe"))
        if fi.Exists then fi.FullName else
        findPath "FSIPath" FSIPath "fsi.exe"
#endif
type FsiArgs =
    FsiArgs of fsiOptions:string list * script:string * scriptArgs:string list with
    static member parse (args:string array) =
        //Find first arg that does not start with - (as these are fsi options that precede the fsx).
        match args |> Array.tryFindIndex (fun arg -> arg.StartsWith("-") = false) with
        | Some(i) ->
            let fsxPath = args.[i]
#if !CORE_CLR
            if fsxPath.EndsWith(".fsx", StringComparison.InvariantCultureIgnoreCase) then
#else
            if fsxPath.EndsWith(".fsx", StringComparison.OrdinalIgnoreCase) then
#endif
                let fsiOpts = if i > 0 then args.[0..i-1] else [||]
                let scriptArgs = if args.Length > (i+1) then args.[i+1..] else [||]
                Choice1Of2(FsiArgs(fsiOpts |> List.ofArray, fsxPath, scriptArgs |> List.ofArray))
            else Choice2Of2(sprintf "Expected argument %s to be the build script path, but it does not have the .fsx extension." fsxPath) 
        | None -> Choice2Of2("Unable to locate the build script path.") 

#if !CORE_CLR
let private FsiStartInfo workingDirectory (FsiArgs(fsiOptions, scriptPath, scriptArgs)) environmentVars =
    (fun (info: ProcessStartInfo) ->
        info.FileName <- fsiPath
        info.Arguments <- String.concat " " (fsiOptions @ [scriptPath] @ scriptArgs)
        info.WorkingDirectory <- workingDirectory
        let setVar k v =
            info.EnvironmentVariables.[k] <- v
        for (k, v) in environmentVars do
            setVar k v
        setVar "MSBuild"  msBuildExe
        setVar "GIT" Git.CommandHelper.gitPath
        setVar "FSI" fsiPath)

/// Creates a ProcessStartInfo which is configured to the F# Interactive.
let fsiStartInfo script workingDirectory env info =
    FsiStartInfo workingDirectory (FsiArgs([], script, [])) env info

/// Run the given buildscript with fsi.exe
let executeFSI workingDirectory script env =
    let (result, messages) =
        ExecProcessRedirected
            (fsiStartInfo script workingDirectory env)
            TimeSpan.MaxValue
    Thread.Sleep 1000
    (result, messages)

/// Run the given build script with fsi.exe and allows for extra arguments to FSI.
let executeFSIWithArgs workingDirectory script extraFsiArgs env =
    let result = ExecProcess (FsiStartInfo workingDirectory (FsiArgs(extraFsiArgs, script, [])) env) TimeSpan.MaxValue
    Thread.Sleep 1000
    result = 0

/// Run the given build script with fsi.exe and allows for extra arguments to the script. Returns output.
let executeFSIWithScriptArgsAndReturnMessages script (scriptArgs: string[]) =
    let (result, messages) =
        ExecProcessRedirected (fun si ->
            FsiStartInfo "" (FsiArgs([], script, scriptArgs |> List.ofArray)) [] si)
            TimeSpan.MaxValue
    Thread.Sleep 1000
    (result, messages)
#endif

open Microsoft.FSharp.Compiler.Interactive.Shell
open System.Reflection

let getScriptAndHash =
    let hashRegex = Text.RegularExpressions.Regex("(?<script>.+)_(?<hash>[a-zA-Z0-9]+)(\.dll|_config\.xml|_warnings\.txt)$", System.Text.RegularExpressions.RegexOptions.Compiled)
    fun fileName ->
        let matched = hashRegex.Match(fileName)
        matched.Groups.Item("script").Value, matched.Groups.Item("hash").Value

type private CacheInfo =
  {
    Provider : ICachingProvider
    ScriptFileName : string
    ScriptHash : string
    AssemblyPath : string
    AssemblyWarningsPath : string
    CacheConfigPath : string
    IsValid : bool
  }

/// gets a cache entry for the given script.
/// We need to consider fsiOptions as they might contain --defines.
let private getCacheInfoFromScript (provider:ICachingProvider) printDetails fsiOptions scriptPath =
    let allScriptContents = getAllScripts scriptPath
    let scriptHash = getScriptHash allScriptContents fsiOptions
    //TODO this is only calculating the hash for the input file, not anything #load-ed
    
    let scriptFileName = Path.GetFileName(scriptPath)
    let fakeDir = Path.Combine(Path.GetDirectoryName scriptPath, ".fake")
    let hashPath = Path.Combine(fakeDir, scriptFileName + "_" + scriptHash)
    let assemblyPath = hashPath + ".dll"
    let assemblyWarningsPath = hashPath + "_warnings.txt"
    let cacheConfigPath = hashPath + "_config.xml"
    let cacheConfig = provider.TryLoadCache cacheConfigPath
#if NETSTANDARD1_5
    let loadContext = AssemblyLoadContext.Default
#endif
    let loadedAssemblies, knownAssemblies, cacheValid =
        let cacheFilesExistAndAreValid =
            File.Exists(assemblyPath) &&
            cacheConfig.IsSome &&
            File.Exists(assemblyWarningsPath) &&
            cacheConfig.Value |> Seq.length > 0
        if cacheFilesExistAndAreValid then
            let loadedAssemblies =
                cacheConfig.Value
                |> Seq.choose (fun assemInfo ->
                    try let assem =
                            if assemInfo.Location <> "" then
#if NETSTANDARD1_5
                                loadContext.LoadFromAssemblyPath(assemInfo.Location)
                            else loadContext.LoadFromAssemblyName(new AssemblyName(assemInfo.FullName))
#else
                                Reflection.Assembly.LoadFrom(assemInfo.Location)
                            else Reflection.Assembly.Load(assemInfo.FullName)
#endif
                        Some(assemInfo, assem)
                    with ex -> if printDetails then tracef "Unable to find assembly %A" assemInfo
                               None)
                |> Seq.toList
            let knownAssemblies =
                loadedAssemblies
                |> List.map (fun (assemInfo, assem) -> assemInfo.FullName, assem)
                |> dict
            let assemVersionValidCount =
                loadedAssemblies
                |> Seq.filter(fun (assemInfo, assem) ->
                    assem.GetName().Version.ToString() = assemInfo.Version)
                |> Seq.length
            loadedAssemblies, knownAssemblies, assemVersionValidCount = Seq.length cacheConfig.Value
        else
            [], dict [], false
    
#if NETSTANDARD1_5
    loadContext.add_Resolving(new Func<AssemblyLoadContext, AssemblyName, Assembly>(fun _ name ->
        let strName = name.FullName
#else
    AppDomain.CurrentDomain.add_AssemblyResolve(new ResolveEventHandler(fun _ ev ->
        let strName = ev.Name
        let name = AssemblyName(strName)
#endif
        match knownAssemblies.TryGetValue(strName) with
        | true, a ->
            if printDetails then tracefn "Redirect assembly load to known assembly: %s" strName
            Choice2Of2 a
        | _ ->
            let token = name.GetPublicKeyToken()
            match loadedAssemblies
                |> Seq.map snd
                |> Seq.tryFind (fun asem ->
                    let n = asem.GetName()
                    n.Name = name.Name &&
                    (isNull token || // When null accept what we have.
                        n.GetPublicKeyToken() = token)) with
            | Some (asem) ->
                traceFAKE "Redirect assembly from '%s' to '%s'" strName asem.FullName
                Choice2Of2 asem
            | _ ->
                if printDetails then traceFAKE "Could not resolve '%s'" strName
                Choice1Of2 name
        |> provider.ResolveAssembly))
    if not cacheValid then provider.Invalidate(cacheConfigPath)
    { Provider = provider
      ScriptFileName = scriptFileName
      ScriptHash = scriptHash
      AssemblyPath = assemblyPath
      AssemblyWarningsPath = assemblyWarningsPath
      CacheConfigPath = cacheConfigPath
      IsValid = cacheValid }

/// because it is used by test code
let nameParser scriptFileName =
    let noExtension = Path.GetFileNameWithoutExtension(scriptFileName)
    let startString = "<StartupCode$FSI_"
    let endString =
      sprintf "_%s%s$%s"
        (noExtension.Substring(0, 1).ToUpper())
        (noExtension.Substring(1))
        (Path.GetExtension(scriptFileName).Substring(1))
    let fullName i = sprintf "%s%s>.$FSI_%s%s" startString i i endString
    let exampleName = fullName "0001"
    let parseName (n:string) =
        if n.Length >= exampleName.Length &&
            n.Substring(0, startString.Length) = startString &&
            n.Substring(n.Length - endString.Length) = endString then
            let num = n.Substring(startString.Length, 4)
            assert (fullName num = n)
            Some (num)
        else None
    exampleName, fullName, parseName

/// Run a script from the cache
let private runScriptCached printDetails cacheInfo out err =
    if printDetails then trace "Using cache"
    let exampleName, fullName, parseName = nameParser cacheInfo.ScriptFileName
    try
        Yaaf.FSharp.Scripting.Helper.consoleCapture out err (fun () ->
#if NETSTANDARD1_5
            let loadContext = AssemblyLoadContext.Default
            let ass = loadContext.LoadFromAssemblyPath(cacheInfo.AssemblyPath)
#else
            let ass = Reflection.Assembly.LoadFrom(cacheInfo.AssemblyPath)
#endif
            match ass.GetTypes()
                  |> Seq.filter (fun t -> parseName t.FullName |> Option.isSome)
                  |> Seq.map (fun t -> t.GetMethod("main@", BindingFlags.InvokeMethod ||| BindingFlags.Public ||| BindingFlags.Static))
                  |> Seq.filter (isNull >> not)
                  |> Seq.tryHead with
            | Some mainMethod ->
              try mainMethod.Invoke(null, [||])
                  |> ignore
                  true
              with
              | ex ->
                  traceError (ex.ToString())
                  false
            | None -> failwithf "We could not find a type similar to '%s' containing a 'main@' method in the cached assembly (%s)!" exampleName cacheInfo.AssemblyPath)
    finally
        try
            traceFAKE "%s" (File.ReadAllText cacheInfo.AssemblyWarningsPath)
        with e -> traceError (e.ToString())

/// Handles a cache store operation, this should not throw as it is executed in a finally block and
/// therefore might eat other exceptions. And a caching error is not critical.
let private handleCaching printDetails (session:IFsiSession) fsiErrorOutput (cacheDir:DirectoryInfo) cacheInfo  =
    try
        let wishName = "FAKE_CACHE_" + Path.GetFileNameWithoutExtension cacheInfo.ScriptFileName + "_" + cacheInfo.ScriptHash
        let d = session.DynamicAssemblyBuilder
        let name = "FSI-ASSEMBLY"
#if NETSTANDARD1_6
        failwith "Wow. Dotnetcore currently doesn't support saving dynamic assemblies. See https://github.com/dotnet/coreclr/issues/1709, https://github.com/dotnet/corefx/issues/4491. As it only hits F# it will probably never be implemented ;). One way to solve this would be to use IKVM.Reflection or Mono.Cecil in FSharp.Compiler.Service. But that's probably a lot of work. Feel free to start :). For now Caching cannot work."
#else
        d.Save(name + ".dll")
#endif
        if not <| Directory.Exists cacheDir.FullName then
            let di = Directory.CreateDirectory cacheDir.FullName
            di.Attributes <- FileAttributes.Directory ||| FileAttributes.Hidden

        let destinationFile = FileInfo(cacheInfo.AssemblyPath)
        let targetDirectory = destinationFile.Directory

        if (not <| targetDirectory.Exists) then targetDirectory.Create()
        if (destinationFile.Exists) then destinationFile.Delete()

        File.WriteAllText(cacheInfo.AssemblyWarningsPath, fsiErrorOutput.ToString())

        try
            // Now we change the AssemblyName of the written Assembly via Mono.Cecil.
            // Strictly speaking this is not needed, however this helps with executing
            // the test suite, as the runtime will only load a single
            // FSI-ASSEMBLY with version 0.0.0.0 by using LoadFrom...
#if NETSTANDARD1_6
            let reader =
                let searchpaths =
                    [ AppContext.BaseDirectory ]
                let resolve name =
                    let n = AssemblyName(name)
                    // Maybe caching provider can already tell us
                    match cacheInfo.Provider.TryLoadCache (cacheInfo.CacheConfigPath) 
                          |> Option.bind (List.tryFind (fun a -> a.FullName = name)) with
                    | Some f -> f.Location
                    | None ->
                        match searchpaths
                              |> Seq.collect (fun p -> Directory.GetFiles(p, "*.dll"))
                              |> Seq.tryFind (fun f -> f.ToLowerInvariant().Contains(n.Name.ToLowerInvariant())) with
                        | Some f -> f
                        | None ->
                            failwithf "Could not resolve '%s'" name
                { new Mono.Cecil.IAssemblyResolver with 
                    member x.Resolve (name : string) =
                        Mono.Cecil.AssemblyDefinition.ReadAssembly(
                            resolve name,
                            new Mono.Cecil.ReaderParameters(AssemblyResolver = x))
                    member x.Resolve (name : string, parms : Mono.Cecil.ReaderParameters) =
                        Mono.Cecil.AssemblyDefinition.ReadAssembly(resolve name, parms)
                    member x.Resolve (name : Mono.Cecil.AssemblyNameReference) =
                        x.Resolve(name.FullName)
                    member x.Resolve (name : Mono.Cecil.AssemblyNameReference, parms : Mono.Cecil.ReaderParameters) =
                        x.Resolve(name.FullName, parms) }
#else
            let reader = new Mono.Cecil.DefaultAssemblyResolver() // see https://github.com/fsharp/FAKE/issues/1084
            reader.AddSearchDirectory (Path.GetDirectoryName fakePath)
            reader.AddSearchDirectory (Path.GetDirectoryName typeof<string option>.Assembly.Location)
#endif
            let readerParams = new Mono.Cecil.ReaderParameters(AssemblyResolver = reader)
            let asem = Mono.Cecil.AssemblyDefinition.ReadAssembly(name + ".dll", readerParams)
            asem.Name <- new Mono.Cecil.AssemblyNameDefinition(wishName, new Version(0,0,1))
            asem.Write(wishName + ".dll")
            File.Move(wishName + ".dll", cacheInfo.AssemblyPath)
        with exn ->
            // If cecil fails we might want to trigger a warning, but you know what?
            // we can continue using the FSI-ASSEMBLY.dll
            traceFAKE "Warning (please open an issue on FAKE and /cc @matthid): %O" exn
            File.Move(name + ".dll", cacheInfo.AssemblyPath)

        for name in [ name; wishName ] do
            for ext in [ ".dll"; ".pdb"; ".dll.mdb" ] do
                if File.Exists(name + ext) then
                    File.Delete(name + ext)

        if cacheInfo.Provider.TrySaveCache(cacheInfo.CacheConfigPath) then
            if printDetails then trace (System.Environment.NewLine + "Saved cache")
    with ex ->
#if NETSTANDARD1_6
        traceFAKE "Caching is not working on dotnetcore: %s" ex.Message
#else
        // Caching errors are not critical, and we shouldn't throw in a finally clause.
        traceFAKE "CACHING ERROR - please open a issue on FAKE and /cc @matthid\n\nError: %O" ex
#endif
        if File.Exists cacheInfo.AssemblyWarningsPath then
            // Invalidates the cache
            try File.Delete cacheInfo.AssemblyWarningsPath with _ -> ()

/// Run a given script uncached, saves the cache if useCache is set to true.
/// deletes any existing caching for the given script.
let private runScriptUncached (useCache, scriptPath, fsiOptions) printDetails cacheInfo out err =
    let options = fsiOptions |> Seq.toArray |> cacheInfo.Provider.MapFsiOptions |> FsiOptions.ofArgs

    let cacheDir = DirectoryInfo(Path.Combine(Path.GetDirectoryName(scriptPath),".fake"))
    if useCache then
        // If we are here that proably means that
        // when trying to load the cached version something went wrong...
        if cacheDir.Exists then
            let oldFiles =
                cacheDir.GetFiles()
                |> Seq.filter(fun file ->
                    let oldScriptName, _ = getScriptAndHash(file.Name)
                    oldScriptName = cacheInfo.ScriptFileName)
                |> Seq.toList

            if not <| List.isEmpty oldFiles then
                if printDetails then trace "Cache is invalid, recompiling"
                oldFiles
                |> List.map (fun file ->
                    try file.Delete(); true
                    // File might be locked (for example when executing the test suite!)
                    with :? UnauthorizedAccessException ->
                        traceFAKE "Unable to access %s" file.FullName
                        false)
                |> List.exists id |> not
                // we could not delete a single file -> cache was not invalidated
                |> function
                    | true ->
                        traceError (sprintf "Unable to invalidate cache for '%s', please delete the .fake folder!" cacheInfo.ScriptFileName)
                    | _ -> ()
            else
                if printDetails then trace "Cache doesn't exist"
        else
            if printDetails then trace "Cache doesn't exist"

    // Contains warnings and errors about the build script.
    let fsiErrorOutput = new System.Text.StringBuilder()
    let session =
      try ScriptHost.Create
            (options, preventStdOut = true,
              fsiErrWriter = ScriptHost.CreateForwardWriter
                ((fun s ->
                    if String.IsNullOrWhiteSpace s |> not then
                        fsiErrorOutput.AppendLine s |> ignore),
                  removeNewLines = true),
              outWriter = out,
              errWriter = err)
      with :? FsiEvaluationException as e ->
          traceError "FsiEvaluationSession could not be created."
          traceError e.Result.Error.Merged
          reraise ()

    try
        try
            session.EvalScript scriptPath
            true
        with :? FsiEvaluationException as eval ->
            traceFAKE "%O" eval
            false
    finally
        // Write Script Warnings & Errors at the end
        let strFsiErrorOutput = fsiErrorOutput.ToString()
        if strFsiErrorOutput <> "" then
            traceFAKE "%O" strFsiErrorOutput
        // Cache in the error case as well.
        if useCache && not cacheInfo.IsValid then
            handleCaching printDetails session fsiErrorOutput cacheDir cacheInfo

/// Run the given FAKE script with fsi.exe at the given working directory. Provides full access to Fsi options and args. Redirect output and error messages.
let internal runFakeWithCache provider printDetails (FsiArgs(fsiOptions, scriptPath, scriptArgs)) env onErrMsg onOutMsg useCache =
    if printDetails then traceFAKE "Running Buildscript: %s" scriptPath
    
    if printDetails then
#if NETSTANDARD1_5
      let loadContext = AssemblyLoadContext.Default
      loadContext.add_Resolving(new Func<AssemblyLoadContext, AssemblyName, Assembly>(fun _ name ->
        let strName = name.FullName
#else
      AppDomain.CurrentDomain.add_AssemblyResolve(new ResolveEventHandler(fun _ ev ->
        let strName = ev.Name
        let name = AssemblyName(strName)
#endif
        trace <| sprintf "FAKE: Trying to resolve %s" strName; null))

    // Add arguments to the Environment
    for (k,v) in env do
      setProcessEnvironVar k v

    // Create an env var that only contains the build script args part from the --fsiargs (or "").
    setEnvironVar "fsiargs-buildscriptargs" (String.Join(" ", scriptArgs))

    let scriptPath =
        if Path.IsPathRooted scriptPath then
            scriptPath
        else
            Path.Combine(getCurrentDirectory(), scriptPath)

    let cacheInfo = getCacheInfoFromScript provider printDetails fsiOptions scriptPath

    use out = ScriptHost.CreateForwardWriter onOutMsg
    use err = ScriptHost.CreateForwardWriter onErrMsg
    if useCache && cacheInfo.IsValid then
        try
            runScriptCached printDetails cacheInfo out err
        with ex ->
            traceFAKE """CACHING WARNING
this might happen after Updates...
please open a issue on FAKE and /cc @matthid ONLY IF this happens reproducibly)

Error: %O""" ex
            // Invalidates the cache
            runScriptUncached (useCache, scriptPath, fsiOptions) printDetails cacheInfo out err
    else
        runScriptUncached (useCache, scriptPath, fsiOptions) printDetails cacheInfo out err

let internal runFAKEScriptWithFsiArgsAndRedirectMessages printDetails fsiArgs env onErrMsg onOutMsg useCache =
    runFakeWithCache Cache.defaultProvider printDetails fsiArgs env onErrMsg onOutMsg useCache

let internal onMessage isError =
    let printer = if isError && TraceListener.importantMessagesToStdErr then eprintf else printf
    printer "%s"

#if !CORE_CLR
/// Run the given buildscript with fsi.exe and allows for extra arguments to the script. Returns output.
let executeBuildScriptWithArgsAndFsiArgsAndReturnMessages script (scriptArgs: string[]) (fsiArgs:string[]) useCache =
    let messages = ref []
    let appendMessage isError msg =
        onMessage isError msg // For the tests to be more realistic
        messages := { IsError = isError
                      Message = msg
                      Timestamp = DateTimeOffset.UtcNow } :: !messages
    let result =
        runFAKEScriptWithFsiArgsAndRedirectMessages
            true (FsiArgs(fsiArgs |> List.ofArray, script, scriptArgs |> List.ofArray)) []
            (appendMessage true) (appendMessage false) useCache
    (result, !messages |> List.rev)

/// Run the given buildscript with fsi.exe and allows for extra arguments to the script. Returns output.
let executeBuildScriptWithArgsAndReturnMessages script (scriptArgs: string[]) useCache =
    executeBuildScriptWithArgsAndFsiArgsAndReturnMessages script scriptArgs [||] useCache
#endif
/// Run the given buildscript with fsi.exe at the given working directory.  Provides full access to Fsi options and args.
let runBuildScriptWithFsiArgsAt printDetails (FsiArgs(fsiOptions, script, scriptArgs)) env useCache =
    runFAKEScriptWithFsiArgsAndRedirectMessages
        printDetails (FsiArgs(fsiOptions, script, scriptArgs)) env
        (onMessage true) (onMessage false)
        useCache

/// Run the given buildscript with fsi.exe at the given working directory.
let runBuildScriptAt printDetails script extraFsiArgs env useCache =
    runBuildScriptWithFsiArgsAt printDetails (FsiArgs(extraFsiArgs, script, [])) env useCache

/// Run the given buildscript with fsi.exe
let runBuildScript printDetails script extraFsiArgs env useCache =
    runBuildScriptAt printDetails script extraFsiArgs env useCache