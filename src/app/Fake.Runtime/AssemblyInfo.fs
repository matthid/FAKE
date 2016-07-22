namespace System
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly: AssemblyTitleAttribute("FAKE - F# Fake.Runtime")>]
[<assembly: AssemblyProductAttribute("FAKE - F# Make")>]
[<assembly: AssemblyVersionAttribute("4.35.1")>]
[<assembly: AssemblyInformationalVersionAttribute("4.35.1")>]
[<assembly: AssemblyFileVersionAttribute("4.35.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "4.35.1"
    let [<Literal>] InformationalVersion = "4.35.1"
