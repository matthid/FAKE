/// New Command line interface for FAKE that utilises Argu.
[<RequireQualifiedAccessAttribute>]
module Cli

open System
open CommandLine

[<Verb("run", HelpText = "Run the given FAKE script.")>]
type RunOptions = {
  [<Option(Separator=',', Required = false, HelpText = "Overwrite some environment variables. Use env1:val1,env2:val2")>] environmentVariables : seq<string>;
  [<Option(HelpText = "Be more verbose in the output (legacy 'PrintDetails' or -pd).")>] verbose : bool;
  [<Option(HelpText = "Use the given string as arguments for FSI. Make sure to escape properly.")>] fsiArgs : string;
  [<Option(HelpText = "Pauses FAKE with a Debugger.Break() near the start (legacy 'Break' or -br).")>] debugScript : bool;
  [<Option(HelpText = "Runs only the specified target and not the dependencies.")>] singleTarget : bool;
  [<Option(HelpText = "Disables caching of compiled script")>] noCache : bool;
  [<Value(0, Required = false, MetaName="script", HelpText = "The build script to run, defaults to build.fsx.")>] script : string option;
  [<Value(1, Required = false, MetaName="target", HelpText = "The the target to run, defaults to the target defined in the build script.")>] target : string option;
}
[<Verb("version", HelpText = "print the current version.")>]
type VersionOptions = {
  [<Option(HelpText = "Be more verbose in the output (legacy 'PrintDetails' or -pd).")>] verbose : bool;
}
