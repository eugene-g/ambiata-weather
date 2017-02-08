module AmbiataWeather.Program

open System
open System.Collections.Generic
open Argu

open Parsing

type CliArgs =
    | [<Mandatory>]Input of string
    | Temp_Min
    | Temp_Max
    | Temp_Mean
    | Observations
    | Distance
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input filename -> "input file."
            | Temp_Min _ -> "calculate minimum temperature."
            | Temp_Max _ -> "calculate maximum temperature."
            | Temp_Mean _ -> "calculate mean temperature."
            | Observations _ -> "calculate number of observations from each observatory."
            | Distance _ -> "calculate total distance travelled."

type Exiter() =
    interface IExiter with
        member __.Name = "exiter"
        member __.Exit (msg,code) =
            Console.WriteLine msg
            if code = ErrorCode.HelpText then
                exit 0
            else exit 1


type FlightStats =
    { minTemp : decimal
      maxTemp : decimal
      meanTemp : decimal

      ///The number of observations from each observatory.
      observations : Dictionary<string,int64>
      distance : bigint
      lastLocation : int64 * int64
      numberOfRecords : int64
    }

// Observatory units.
//
// AU (celsius, km)
// US (fahrenheit, miles)
// FR (kelvin, m)
// _ (kelvin, km)


/// Resolves temperature converter for given observatory.
/// Converter produces result in Kelvins.
/// Conversion formulas are taken here: https://en.wikipedia.org/wiki/Conversion_of_units_of_temperature
let resolveTemperatureConverter = function
    | AU -> fun celsius -> celsius + 273.15M |> decimal
    | US -> fun fahrenheit -> (fahrenheit + 459.67M) * (5.0M/9.0M)
    | _ -> fun value -> value |> decimal


/// Resolves location conveter for given observatory.
/// Converter produces result in meters.
/// There is minor loss of fraction regarding miles to meters conversion, to keep Record:location as int64*int64.
let resolveLocationConverter = function
    | US -> fun miles -> miles * 1609L
    | FR -> id
    | _ -> fun km -> km  * 1000L


let normalizeTemperature temp observatory =
    let convert = resolveTemperatureConverter observatory
    convert temp
    |> decimal


let normalizeLocation (x, y) observatory =
    let convert = resolveLocationConverter observatory
    (convert x, convert y) 

/// Normalizes units for temperature and location.
/// Returns None if temperature is invalid.
let normalize (record : Record) =

    let normalizedTemp = normalizeTemperature record.temperature record.observatory
    if normalizedTemp < 0M then None
    else
    Some
        { record with
            temperature = normalizedTemp
            location = normalizeLocation record.location record.observatory
        }


/// Taken from FSharpx.Collections
/// https://github.com/fsprojects/FSharpx.Collections/blob/master/src/FSharpx.Collections/Collections.fs#L140-140
let tryHead (source : seq<_>) = 
    use e = source.GetEnumerator()
    if e.MoveNext()
    then Some(e.Current)
    else None


/// Calculates mean value.
/// Note: if intervals between measurement is not regular, it would be much better to "normalize" values by time.
/// <param name="acc">Current mean.</param>
/// <param name="x">New value.</param>
/// <param name="n">Total number of values, used to calculate current mean.</param>
let mean acc x n =
    (acc * (decimal n) + (decimal x)) / decimal (n + 1L)


/// Calculate Euclidian distance between 2 points.
let calculateDistance (x1, y1) (x2, y2) =
    (pown (x2 - x1) 2) + (pown (y2 - y1) 2)
    |> double
    |> sqrt
    |> bigint

let observatoryToString = function
    | AU -> "AU"
    | US -> "US"
    | FR -> "FR"
    | Other name -> name


let createInitialStats = function
    | None -> failwith "no records were found"
    | Some record ->
        { minTemp = record.temperature
          maxTemp = record.temperature
          meanTemp = record.temperature
          distance = bigint 0
          observations = Dictionary<string,int64>()
          lastLocation = record.location
          numberOfRecords = 1L
        }


let calculateFlightStats (input : Record seq) =

    let aux (stats : FlightStats) (record : Record) =
        let observatory = record.observatory |> observatoryToString

        match stats.observations.TryGetValue observatory with
        | true, result -> stats.observations.[observatory] <- stats.observations.[observatory] + 1L
        | _ -> stats.observations.Add(observatory, 1L)

        { stats with
              minTemp = min record.temperature stats.minTemp
              maxTemp = max record.temperature stats.maxTemp
              meanTemp = mean stats.meanTemp record.temperature stats.numberOfRecords
              distance = stats.distance + calculateDistance stats.lastLocation record.location
              numberOfRecords = stats.numberOfRecords + 1L
        }

    let initialStats = 
        tryHead input
        |> createInitialStats

    // we should skip 1 value, because it was used for initial stats
    Seq.fold aux initialStats (Seq.skip 1 input)

let printStats (args : ParseResults<CliArgs>) (input : FlightStats) =

    if args.Contains<@ Temp_Min @> then
        printfn "Min temperature: %M" input.minTemp

    if args.Contains<@ Temp_Max @> then
        printfn "Max temperature: %M" input.maxTemp

    if args.Contains<@ Temp_Mean @> then
        printfn "Mean temperature: %M" input.meanTemp

    if args.Contains<@ Observations @> then
        printfn "Number of observations:"
        for KeyValue(key, value) in input.observations do
            printfn "    %s - %i" key value

    if args.Contains<@ Distance @> then
        printfn "Total distance: %A meters" input.distance


[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArgs>(errorHandler=Exiter())

    let args = parser.ParseCommandLine(argv)
    
    args.GetResult<@ Input @>
    |> parseFile
    |> Seq.map normalize
    |> Seq.choose id
    |> calculateFlightStats
    |> printStats args

    0
