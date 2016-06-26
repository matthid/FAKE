open System
open Fake.Environment
open Fake.String
open Fake.Trace
open Fake.BuildServer
open Fake.Fsi
open System.IO
open CommandLine

let printVersion() =
    traceFAKE "FakePath: %s" fakePath
    traceFAKE "%s" fakeVersionStr


let printEnvironment cmdArgs args =
    printVersion()

    if buildServer = LocalBuild then
        trace localBuildLabel
    else
        tracefn "Build-Version: %s" buildVersion

    if cmdArgs |> Array.length > 1 then
        traceFAKE "FAKE Arguments:"
        args 
          |> Seq.map fst
          |> Seq.iter (tracefn "%A")

    log ""
    //traceFAKE "FSI-Path: %s" fsiPath
    //traceFAKE "MSBuild-Path: %s" msBuildExe

let containsParam param = Seq.map toLower >> Seq.exists ((=) (toLower param))

let paramIsHelp param = containsParam param ["help"; "?"; "/?"; "-h"; "--help"; "/h"; "/help"]

let buildScripts = System.IO.Directory.EnumerateFiles(currentDirectory(), "*.fsx") |> Seq.toList

// http://stackoverflow.com/questions/298830/split-string-containing-command-line-parameters-into-string-in-c-sharp/298990#298990
let splitBy f (s:string) =
  seq {
    let mutable nextPiece = 0
    for c, i in s |> Seq.mapi (fun i c -> c, i) do
      if f c then
        yield s.Substring(nextPiece, i - nextPiece)
        nextPiece <- i + 1
  }
let trim (s:string) = s.Trim()
let trimMatchingQuotes quote (s:string) =
  if s.Length >= 2 && s.[0] = quote && s.[s.Length - 1] = quote
  then s.Substring(1, s.Length - 2)
  else s

let splitCommandLine s =
  let mutable inQuotes = false
  s
  |> splitBy (fun c -> if c = '\"' then inQuotes <- not inQuotes
                       not inQuotes && c = ' ')
  |> Seq.map (trim >> trimMatchingQuotes '\"')
  |> Seq.filter (System.String.IsNullOrEmpty >> not)

[<EntryPoint>]
let main (args:string[]) =
  let mutable exitCode = 0
  try
    try
      //AutoCloseXmlWriter <- true
      //let cmdArgs = System.Environment.GetCommandLineArgs()
      let result = Parser.Default.ParseArguments<Cli.RunOptions, Cli.VersionOptions> args
      match result with
      | :? Parsed<obj> as command ->
        match command.Value with
        | :? Cli.VersionOptions as opts ->
          //let isVersion = false // version switch is no a different 'verb'
          printVersion()
        | :? Cli.RunOptions as opts ->
          //Break to allow a debugger to be attached here
          if opts.debugScript then
              Diagnostics.Debugger.Launch() |> ignore
              Diagnostics.Debugger.Break() |> ignore

          //Boot and version force us to ignore other args, so check for them and handle.
          //let isBoot, bootArgs = false, [] // no boot support (whatever that is)
          let printDetails = opts.verbose

          //match isBoot with
          ////Boot.
          //| true ->
          //    let handler = Boot.HandlerForArgs bootArgs//Could be List.empty, but let Boot handle this.
          //    handler.Interact()
          //
          ////Try and run a build script! 
          //| false ->

          traceStartBuild()
          if printDetails then printVersion()

          //Maybe log.
          //match fakeArgs.TryGetResult <@ Cli.LogFile @> with
          //| Some(path) -> addXmlListener path
          //| None -> ()

          //Combine the key value pair vars and the flag vars.
          let envVars =
              seq {
                yield! 
                  opts.environmentVariables
                  |> Seq.map (fun s -> let split = s.Split(':') in split.[0], split.[1])
                if opts.singleTarget then yield "single-target", "true"
                if opts.target.IsSome then yield "target", opts.target.Value
              }
              //seq { yield! fakeArgs.GetResults <@ Cli.EnvFlag @> |> Seq.map (fun name -> name, "true")
              //      yield! fakeArgs.GetResults <@ Cli.EnvVar @>
              //      if fakeArgs.Contains <@ Cli.Single_Target @> then yield "single-target", "true"
              //      if args.Target.IsSome then yield "target", args.Target.Value }

          //Get our fsiargs from somewhere!
          let fsiArgs = 
              match
                  splitCommandLine opts.fsiArgs |> Seq.toList,
                  opts.script,
                  List.isEmpty buildScripts with

              //TODO check for presence of --fsiargs with no args?  Make attribute for UAP?

              //Use --fsiargs approach.
              | x::xs, _, _ ->
                  match FsiArgs.parse (x::xs |> Array.ofList)  with
                  | Choice1Of2(fsiArgs) -> fsiArgs
                  | Choice2Of2(msg) -> failwith (sprintf "Unable to parse --fsiargs.  %s." msg)

              //Script path is specified.
              | [], Some(script), _ -> FsiArgs([], script, [])

              //No explicit script, but have in working directory.
              | [], None, false -> FsiArgs([], List.head buildScripts, [])

              //Noooo script anywhere!
              | [], None, true -> failwith "Build script not specified on command line, in fsi args or found in working directory."
                    
          //TODO if printDetails then printEnvironment cmdArgs args

          let useCache = not opts.noCache
          if not (runBuildScriptWithFsiArgsAt printDetails fsiArgs envVars useCache) then exitCode <- 1
          else if printDetails then log "Ready."

      | :? NotParsed<obj> ->
        exitCode <- 1

    with
    | exn -> 
        if exn.InnerException <> null then
            sprintf "Build failed.\nError:\n%s\nInnerException:\n%s" exn.Message exn.InnerException.Message
            |> traceError
            //printUsage()
        else
            sprintf "Build failed.\nError:\n%s" exn.Message
            |> traceError
            //printUsage()

        //let isKnownException = exn :? FAKEException
        //if not isKnownException then
        //    sendTeamCityError exn.Message

        exitCode <- 1

    //killAllCreatedProcesses()

  finally
    traceEndBuild()
    //if !TargetHelper.ExitCode.exitCode <> 0 then exit !TargetHelper.ExitCode.exitCode
#if !NETSTANDARD1_5
    if Environment.ExitCode <> 0 then exitCode <- Environment.ExitCode
#endif
  exitCode
