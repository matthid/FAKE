(* -- Fake Dependencies paket-inline
source https://nuget.org/api/v2
source ..\..\..\nuget\dotnetcore

nuget Fake.Core.Context prerelease
-- Fake Dependencies -- *)
#load "./.fake/context-exists.fsx/loadDependencies.fsx"

printfn "loading context"
let context = Fake.Core.Context.forceFakeContext()
printfn "got: %A" context
