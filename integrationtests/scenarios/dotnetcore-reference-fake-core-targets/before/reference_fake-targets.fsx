(* -- Fake Dependencies paket-inline
source https://nuget.org/api/v2
source .\..\..\..\..\..\..\nuget\dotnetcore

nuget Fake.Core.Targets prerelease
nuget Microsoft.FSharp.Core.netcore
-- Fake Dependencies -- *)

printfn "before load"

#load "./.fake/reference_fake-targets.fsx/loadDependencies.fsx"

printfn "test_before open"

open Fake.Core
open Fake.Core.Targets
open Fake.Core.TargetOperators

printfn "test_before targets"
Target "Start" (fun _ -> ())

Target "TestTarget" (fun _ ->
    printfn "Starting Build."
    Trace.traceFAKE "Some Info from FAKE"
    printfn "Ending Build."
)

"Start"
  ==> "TestTarget"

printfn "before run targets"

RunTargetOrDefault "TestTarget"
