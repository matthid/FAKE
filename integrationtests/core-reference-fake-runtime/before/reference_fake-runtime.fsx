(* -- Fake Dependencies paket-inline
source https://nuget.org/api/v2
source .\..\..\..\..\..\nuget\dotnetcore

nuget Fake.Runtime prerelease
nuget Microsoft.FSharp.Core.netcore
-- Fake Dependencies -- *)
#load "./.fake/reference_fake-runtime.fsx/loadDependencies.fsx"

open Fake.Runtime

printfn "Starting Build."
Trace.traceFAKE "Some Info from FAKE"
printfn "Ending Build."