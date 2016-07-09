(* -- Fake Dependencies paket-inline
source https://nuget.org/api/v2

nuget System.Xml.XmlDocument ~> 4.0
nuget System.Security.Cryptography.Algorithms ~> 4.0
nuget Microsoft.FSharp.Core.netcore
-- Fake Dependencies -- *)
#load "./.fake/unusedDependencies.fsx/loadDependencies.fsx"

open System.Xml


printfn "Starting Build."