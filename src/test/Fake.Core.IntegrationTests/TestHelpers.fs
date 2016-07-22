[<AutoOpen>]
module Fake.Core.IntegrationTests.TestHelpers

open Fake.IO.FileSystem
open Fake.IO.FileSystem.Operators
open System
open NUnit.Framework
open System
open System.IO

let fakeToolPath = Path.getFullName(__SOURCE_DIRECTORY__ + "../../../../nuget/dotnetcore/Fake.netcore/current/Fake.netcore.exe")
let integrationTestPath = Path.getFullName(__SOURCE_DIRECTORY__ + "../../../../integrationtests/scenarios")
let scenarioTempPath scenario = integrationTestPath @@ scenario @@ "temp"
let originalScenarioPath scenario = integrationTestPath @@ scenario @@ "before"

let prepare scenario =
    let originalScenarioPath = originalScenarioPath scenario
    let scenarioPath = scenarioTempPath scenario
    Directory.Delete(scenarioPath, true)
    Directory.ensure scenarioPath
    Shell.CopyDir scenarioPath originalScenarioPath (fun _ -> true)

let directFakeInPath command scenarioPath =
    let result =
        Fake.ProcessHelper.ExecProcessAndReturnMessages (fun info ->
          info.FileName <- fakeToolPath
          info.WorkingDirectory <- scenarioPath
          info.Arguments <- command) (System.TimeSpan.FromMinutes 5.)
    if result.ExitCode <> 0 then 
        let errors = String.Join(Environment.NewLine,result.Errors)
        printfn "%s" <| String.Join(Environment.NewLine,result.Messages)
        failwith errors
    String.Join(Environment.NewLine,result.Messages)

let directFake command scenario =
    directFakeInPath command (scenarioTempPath scenario)

let fake command scenario =
    prepare scenario

    directFake command scenario

let fakeRun scriptName scenario =
    fake (sprintf "--verbose run %s" scriptName) scenario |> ignore
