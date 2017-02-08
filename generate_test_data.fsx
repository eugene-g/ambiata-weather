open System
open System.IO
open System.Diagnostics

(*

Test data generation strategy.

For each batch following statements are true:

    - there is one observatory
    - datetime sequence is monotonically increasing
    - temperature is random, between -50 and 100
    - coordinates are random
    - there is one malformed line before each batch
    - there is one empty line after each batch

*)


let batchSize = 10000
let outOfOrderFreq = 1000 // 1 line in outOfOrderFreq will be out of order
let malformedLineFreq = 800 // 1 line in malformedLineFreq will be malformed (but not empty)
let maxCoord = 100
let maxTemp = 100 // max value in the observatory units

let output =  Path.GetFullPath @".\testdata.txt"


let observatories = [|"AU"; "US"; "FR"; "RU"; "DE"; "Unknown"|]

if fsi.CommandLineArgs.Length <> 2 then
    printfn "USAGE:"
    printfn "   generate_test_data.fsx numberOfBatches"
    printfn "   batch size is %i" batchSize
    exit 0

let getNumberOfBatches =
    match Int32.TryParse(fsi.CommandLineArgs.[1]) with
    | (true, result) -> result
    | _ ->
        printfn "invalid number of batches"
        exit 0

let rnd = new Random();

let getRandomDay() =
    let origin = new DateTime(2000, 1, 1)
    let range = (DateTime.Today - origin).Days
    rnd.Next(range)
    |> double
    |> origin.AddDays


/// Generates monotonically increasing sequence of datetimes, except the lines which are out of order.
/// Sequence length is equal to batchSize.
let datetimeGen =
    seq {
        let origin = getRandomDay()
        for i in 1..batchSize ->
            if i % outOfOrderFreq = 0 then origin.AddMinutes(-i |> double)
            else origin.AddMinutes(i |> double)
    }

let locationGen =
    seq {
        while true do
            yield (rnd.Next(maxCoord), rnd.Next(maxCoord))
    }

let tempGen =
    seq { while true do yield rnd.Next(-50, 100) }


let getRandomObservatory() =
    observatories.[rnd.Next() % observatories.Length]


let formatDatetime (datetime : DateTime) = 
    datetime.ToString("yyyy-MM-ddTHH:mm")

let formatLocation (x,y) =
    sprintf "%i,%i" x y

let malformedLine() =
    let date = getRandomDay() |> formatDatetime
    let observatory = getRandomObservatory()
    let location = formatLocation (rnd.Next(maxCoord + maxCoord), rnd.Next(maxCoord))
    let temp = rnd.Next(-50, 100)

    sprintf "%s|%s|%s|%s|%s|%i" date observatory location location location temp

let recordsGen =
    seq {
        yield malformedLine()

        let observatory = getRandomObservatory()
        for time, coord, temp in Seq.zip3 datetimeGen locationGen tempGen do
            yield sprintf "%s|%s|%i|%s" (formatDatetime time) (formatLocation coord) temp observatory

        yield ""
    }


let generate numberOfBatches =
    seq {
        for i in 1..numberOfBatches do
            Console.CursorLeft <- 0
            Console.CursorTop <- Console.CursorTop - 1

            printfn "writing batch %i out of %i..." i numberOfBatches
            yield! [for i in Seq.take batchSize recordsGen -> i]
    }


printfn "creating %i batches of size %i" getNumberOfBatches batchSize

printfn ""
let sw = new Stopwatch()
sw.Start()

File.WriteAllLines(output, generate getNumberOfBatches)

printfn "Done! See %s for results." output
printfn "Execution time: %is" (sw.ElapsedMilliseconds / (int64 1000))