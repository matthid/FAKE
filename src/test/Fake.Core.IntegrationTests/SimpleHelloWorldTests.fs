module Fake.Core.IntegrationTests.SimpleHelloWorldTests

open Fake
open System
open NUnit.Framework
open System
open System.IO
open System.Diagnostics

[<Test>]
let ``simple printfn hello world``() = 
    fakeRun "hello_world.fsx" "dotnetcore-no-dependencies-hello-world"

[<Test>]
let ``simple compilation error``() =
    try
        fakeRun "fail-to-compile.fsx" "dotnetcore-simple-failed-to-compile"
        Assert.Fail ("Expected an compilation error and a nonzero exit code!")
    with e -> ()

[<Test>]
let ``simple runtime error``() =
    try
        fakeRun "runtime-error.fsx" "dotnetcore-simple-runtime-error"
        Assert.Fail ("Expected an runtime error and a nonzero exit code!")
    with e -> ()

[<Test>]
let ``reference fake runtime``() = 
    fakeRun "reference_fake-runtime.fsx" "dotnetcore-reference-fake-runtime"

[<Test>]
let ``use external paket.dependencies``() = 
    fakeRun "use_external_dependencies.fsx" "dotnetcore-use-external-paket-dependencies"