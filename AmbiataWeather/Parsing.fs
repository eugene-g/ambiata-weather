namespace AmbiataWeather

open System
open System.IO
open FParsec

//<timestamp>|<location>|<temperature>|<observatory>
//e.g. 2014-12-31T13:44|10,5|243|AU

[<AutoOpen>]
module Parsing =

    type Observatory =
        | AU
        | US
        | FR
        | Other of string

    let createObservatory (name : string) =
        match name.ToUpper() with
        | "AU" -> AU
        | "US" -> US
        | "FR" -> FR
        | x -> x.ToUpperInvariant() |> Other

    /// Represents successfully parsed line.
    type Record =
        {
            datetime : DateTime
            location : int64*int64
            temperature : decimal
            observatory : Observatory
        }

    /// Returns parsing result if parsing was successful. Returns Option.None otherwise.
    let tryGetResult =
        function
        | Success (value, _, _) -> Some value
        | _ -> None


    // Following UserState and Parser declarations are workaround for "value restriction" compiler error.
    // For details, see http://www.quanttec.com/fparsec/tutorial.html#fs-value-restriction
    type UserState = unit
    type Parser<'t> = Parser<'t, UserState>

    let delimiter = '|'
    let isNotDelimiter x = x <> delimiter

    /// Parses timestamp value, which could be in the any .NET-compatible format, e.g. 2014-12-31T13:44
    let pdatetime : Parser<_> =
        fun stream ->
            let candidate = stream.ReadCharsOrNewlinesWhile (isNotDelimiter, false)
            match DateTime.TryParse candidate with
                | true, value -> Reply(value)
                | false, _ -> Reply(Error, messageError "not a datetime")

    let plocation : Parser<_> =
        let x = spaces >>. pint64 .>> spaces
        let y = spaces >>. pint64 .>> eof
        let sep = pstring ","
        x .>> sep .>>. y

    let ptemperature : Parser<_> =
        spaces >>. pint64 .>> spaces .>> eof
        |>> decimal

    let pobservatory : Parser<_> =
        spaces >>. many1Satisfy Char.IsLetterOrDigit .>> spaces .>> eof
        |>> createObservatory

    let (|Datetime|_|) part =
        run pdatetime part
        |> tryGetResult

    let (|Location|_|) part =
        run plocation part
        |> tryGetResult

    let (|Temperature|_|) part =
        run ptemperature part
        |> tryGetResult

    let (|Observatory|_|) part =
        run pobservatory part
        |> tryGetResult

    let parseLine (line : string) =
        match line.Split(delimiter) with
        | [|Datetime datetime; Location location; Temperature temperature; Observatory observatory|] -> 
            Some {
                datetime=datetime
                location=location
                temperature=temperature
                observatory=observatory }
        | _ -> None

    let parseFile (path : string) =
        if not <| File.Exists path then
            invalidArg "path" <| sprintf "file %s doesn't exists" path

        File.ReadLines(path)
        |> Seq.map parseLine
        |> Seq.choose id