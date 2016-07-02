open System
open Fake
open Fake.Environment
open Fake.String
open Fake.Trace
open Fake.BuildServer
open Fake.Fsi
open System.IO
open Argu

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

let buildScripts = System.IO.Directory.EnumerateFiles(getCurrentDirectory(), "*.fsx") |> Seq.toList

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


let handleCli (results:ParseResult<Cli.FakeArgs>) =

  let mutable exitCode = 0
  let printDetails = results.Contains <@ Cli.FakeArgs.Verbose @>
  if results.Contains <@ Cli.FakeArgs.Version @> then
    printVersion()
  results.IterResult (<@ Cli.FakeArgs.Run @>, fun runArgs ->
    if runArgs.Contains <@ Cli.RunArgs.Debug @> then
      Diagnostics.Debugger.Launch() |> ignore
      Diagnostics.Debugger.Break() |> ignore

    try
      try
        //AutoCloseXmlWriter <- true
        //let cmdArgs = System.Environment.GetCommandLineArgs()
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
                runArgs.GetResults(<@ Cli.RunArgs.EnvironmentVariable @>)
                //|> Seq.map (fun s -> let split = s.Split(':') in split.[0], split.[1])
              if runArgs.Contains <@ Cli.RunArgs.SingleTarget @> then yield "single-target", "true"
              if runArgs.Contains <@ Cli.RunArgs.Target @> then yield "target", runArgs.GetResult <@ Cli.RunArgs.Target @>
            }
            //seq { yield! fakeArgs.GetResults <@ Cli.EnvFlag @> |> Seq.map (fun name -> name, "true")
            //      yield! fakeArgs.GetResults <@ Cli.EnvVar @>
            //      if fakeArgs.Contains <@ Cli.Single_Target @> then yield "single-target", "true"
            //      if args.Target.IsSome then yield "target", args.Target.Value }

        //Get our fsiargs from somewhere!
        let fsiArgLine = if runArgs.Contains <@ Cli.RunArgs.FsiArgs @> then runArgs.GetResult <@ Cli.RunArgs.FsiArgs @> else ""
        let s = if runArgs.Contains <@ Cli.RunArgs.Script @> then Some (runArgs.GetResult <@ Cli.RunArgs.Script @>)  else None
        let fsiArgs = 
            match
                splitCommandLine fsiArgLine |> Seq.toList,
                s,
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
        let useCache = not (runArgs.Contains <@ Cli.RunArgs.NoCache @>)
        if not (FakeRuntime.prepareAndRunScript printDetails fsiArgs envVars useCache) then exitCode <- 1
        else if printDetails then log "Ready."
      with
      | exn ->
          if printDetails then
              sprintf "Build failed.\nError:\n%O" exn
              |> traceError
          else
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
  )
  exitCode

[<EntryPoint>]
let main (args:string[]) =
  let mutable exitCode = 0
  let parser = ArgumentParser.Create<Cli.FakeArgs>("fake")
  try
    let results = parser.Parse(args)
    exitCode <- handleCli results
  with
  | :? ArguParseException as e ->
    printfn "%s" e.Message
    exitCode <- 1
#if !NETSTANDARD1_5
  //if !TargetHelper.ExitCode.exitCode <> 0 then exit !TargetHelper.ExitCode.exitCode
  if Environment.ExitCode <> 0 then exitCode <- Environment.ExitCode
#endif
  exitCode
