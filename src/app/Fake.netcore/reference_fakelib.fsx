(* -- Fake Dependencies paket-inline
source https://nuget.org/api/v2
source C:\PROJ\FAKE\src\app\Fake.Runtime\bin\Debug
source C:\PROJ\FAKE\lib\nupgks\paket.core.netcore\3.4.0-alpha002
source C:\PROJ\FAKE\lib\nupgks\fsharp.compiler.service.netcore\1.0.0-alpha-00002
source C:\PROJ\FAKE\lib\nupgks\mono.cecil\0.9.6
source C:\PROJ\FAKE\lib\nupgks\chessie\0.5.2

nuget Fake.Runtime 1.0.0-alpha4
nuget System.Xml.XmlDocument ~> 4.0
nuget System.Security.Cryptography.Algorithms ~> 4.0
nuget Microsoft.FSharp.Core.netcore
-- Fake Dependencies -- *)
#load "./.fake/reference_fakelib.fsx/loadDependencies.fsx"

open Fake.Runtime



printfn "Starting Build."
Trace.traceFAKE "Some Info from FAKE"
printfn "Ending Build."