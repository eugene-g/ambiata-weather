#r @"packages/FAKE/tools/FakeLib.dll"

open System
open System.IO
open System.Text

open Fake
open Fake.Testing

// Properties

let testSuite = FullName @".\AmbiataWeather.TestSuite\bin\Debug\AmbiataWeather.TestSuite.dll"


// Helpers

let buildSolutionTarget target =
    let setParams defaults =
        { defaults with
            Verbosity = Some(Minimal)
            Targets = [target]
            Properties =
                [
                    "Configuration", "Debug"
                    "Platform", "x64"
                ]
         }
    build setParams ".\AmbiataWeather.sln"


// Targets

Target "RestorePackages" <| fun _ ->
    Paket.Restore id

Target "Build" <| fun _ ->
    buildSolutionTarget "Build"

Target "Clean" <| fun _ ->
    buildSolutionTarget "Clean"

Target "RunTests" <| fun _ ->
    testSuite
    |> Seq.singleton
    |> NUnit3
        (fun p -> { p with Framework = NUnit3Runtime.Net45 })


"Clean"
    ==> "RestorePackages"
    ==> "Build"
    ==> "RunTests"

RunTargetOrDefault "RunTests"