#if FAKE_DEPENDENCIES
#r "paket:
storage: none
source https://nuget.org/api/v2
source ../../../nuget/dotnetcore
//source https://ci.appveyor.com/nuget/paket

nuget Fake.Runtime prerelease
nuget FSharp.Core prerelease"
#endif
open klajsdhgfasjkhd

printfn asd
Trace.traceFAKE "Some Info from FAKE"
printfn "Ending Build."