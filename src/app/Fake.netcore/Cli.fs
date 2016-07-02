/// New Command line interface for FAKE that utilises Argu.
[<RequireQualifiedAccessAttribute>]
module Cli

open System
open Argu


type RunArgs =
  | [<First>][<Mandatory>][<CliPrefix(CliPrefix.None)>][<AltCommandLine("")>] Script of string
  | [<AltCommandLine("-t")>] Target of string
  | [<AltCommandLine("-e")>] EnvironmentVariable of string * string
  | [<AltCommandLine("-d")>] Debug
  | [<AltCommandLine("-s")>] SingleTarget
  | [<AltCommandLine("-n")>] NoCache
  | [<Rest>] FsiArgs of string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Script _ -> "Specify the script to run."
      | EnvironmentVariable _ -> "Set an environment variable."
      | FsiArgs _ -> "Arguments passed to the f# interactive."
      | Debug _ -> "Debug the script (set a breakpoint at the start)."
      | SingleTarget _ -> "Run only the specified target."
      | Target _ -> "The target to run."
      | NoCache _ -> "Disable caching of the compiled script."

type FakeArgs =
  | Version
  | (*[<Inherit>]*) [<AltCommandLine("-v")>] Verbose
  | [<CliPrefix(CliPrefix.None)>] Run of Argu.ParseResult<RunArgs>
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Version _ -> "Prints the version."
      | Verbose _ -> "More verbose output."
      | Run _ -> "Runs a build script."