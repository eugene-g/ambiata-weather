module TestSuite

open System

open FParsec
open NUnit.Framework
open Swensen.Unquote

open AmbiataWeather


let isSuccess =
    function
    | Success _ -> true
    | Failure _ -> false

let isFailure x = not <| isSuccess x

/// Helper method to define locations.
let loc(x, y) = (int64 x, int64 y)


[<Test>]
let ``datetime invalid values`` () =

    let isRunParserFails input = isFailure <| run Parsing.pdatetime input

    test <@ isRunParserFails "" @>
    test <@ isRunParserFails "1234" @>
    test <@ isRunParserFails "foobar" @>
    test <@ isRunParserFails "2000-122-31T13:44" @>
    test <@ isRunParserFails "2000-31T13:44" @>
    test <@ isRunParserFails "2000-12-31T25:44" @>
    test <@ isRunParserFails "2014-12-31T13:44  foobar" @>
    test <@ isRunParserFails "2016-12-31T23:60" @> // leap second is not supported by .NET


[<Test>]
let ``datetime`` () =

    let input = ["2014-12-31T13:44"; "     2014   -12-31T13:44"; "2014-12-31T13:44   "; "2014-12-31T13:  44"]
    let expected = new DateTime(year = 2014, month = 12, day = 31, hour = 13, minute = 44, second = 0)

    let result =
        Seq.map (run Parsing.pdatetime >> Parsing.tryGetResult) input
        |> Seq.choose id

    test <@ Seq.length result = Seq.length input @>
    test <@ Seq.isEmpty <| Seq.filter ((<>) expected) result @>


[<Test>]
let ``location invalid values`` () =

    let isRunParserFails input = isFailure <| run Parsing.plocation input
    
    test <@ isRunParserFails "" @>
    test <@ isRunParserFails "1234" @>
    test <@ isRunParserFails "1,2,3" @>
    test <@ isRunParserFails "(1,2)" @>
    test <@ isRunParserFails "foobar" @>


[<Test>]
let ``location`` () =

    // ARRANGE
    let input = ["1,2"; "3,   4"; "5    ,6"; "7,8   "; "    9,10" ]
    let expected =
        ([1L..2L..9L], [2L..2L..10L])
        ||> Seq.zip

    // ACT
    let result =
        input
        |> Seq.map (run Parsing.plocation >> Parsing.tryGetResult)
        |> Seq.choose id

    // ASSERT
    let isMissing x = not <| Seq.contains x expected
    let missingValues = Seq.filter isMissing result

    test <@ Seq.isEmpty missingValues @>


[<Test>]
let ``temperature invalid values`` () =

    let isRunParserFails input = isFailure <| run Parsing.ptemperature input

    
    test <@ isRunParserFails "" @>
    test <@ isRunParserFails "foobar" @>
    test <@ isRunParserFails "123foo" @>
    test <@ isRunParserFails "foo123" @>
    test <@ isRunParserFails "1  2" @>
    test <@ isRunParserFails "  1  2" @>
    test <@ isRunParserFails "1  2  " @>


[<Test>]
let ``temperature`` () =

    let input = [
                "42"     ;
                "   42"  ;
                "  42   ";
                "42   "  ]
    let expected = 42M

    let result =
        Seq.map (run Parsing.ptemperature >> Parsing.tryGetResult) input
        |> Seq.choose id

    test <@ Seq.length result = Seq.length input @>
    test <@ Seq.isEmpty <| Seq.filter ((<>) expected) result @>

[<Test>]
let ``temperature negative`` () =

    let input = [
                "-42"     ;
                "   -42"  ;
                "  -42   ";
                "-42   "  ]
    let expected = -42M

    let result =
        Seq.map (run Parsing.ptemperature >> Parsing.tryGetResult) input
        |> Seq.choose id

    test <@ Seq.length result = Seq.length input @>
    test <@ Seq.isEmpty <| Seq.filter ((<>) expected) result @>


[<Test>]
let ``observatory invalid values`` () =

    let isRunParserFails input = isFailure <| run Parsing.ptemperature input

    test <@ isRunParserFails "" @>
    test <@ isRunParserFails "foo bar" @>


[<Test>]
let ``observatory`` () =
    // ARRANGE
    let input = ["AU"; "Au"; "au"; "FR"; "US"; "  US"; "   US   "; "US   "; "De"; "RU  "; "   en" ]
    let expected = [AU; AU; AU; FR; US; US; US; US; Other "DE"; Other "RU" ; Other "EN"]

    // ACT
    let result =
        Seq.map (run Parsing.pobservatory >> Parsing.tryGetResult) input
        |> Seq.choose id

    // ASSERT
    let zipped = Seq.zip expected result
    let isDifferent (x : Observatory, y : Observatory) =
        x <> y

    test <@ Seq.length result = Seq.length expected @>
    test <@ Seq.filter isDifferent zipped 
            |> Seq.isEmpty @>


[<Test>]
let ``parseLine invalid values`` () =

    let emptyLine = ""
    let extraPart = "2014-12-31T13:44|10,5|243|AU|AU"
    let missingPart = "2014-12-31T13:44|243|AU"
    let wrongPart = "2014-12-31T13:44|243|AU|FR"
    let reordered = "243|2014-12-31T13:44|10,11|FR"

    let isNone x =
        x
        |> Parsing.parseLine
        |> Option.isNone

    test <@ isNone emptyLine @>
    test <@ isNone extraPart @>
    test <@ isNone missingPart @>
    test <@ isNone wrongPart @>
    test <@ isNone reordered @>


[<Test>]
let ``parseLine`` () =

    let result = Parsing.parseLine "2014-12-31T13:44|10,5|243|AU"
    let expected =
          { datetime=new DateTime(year = 2014, month = 12, day = 31, hour = 13, minute = 44, second = 0)
            location=(10L, 5L)
            temperature=243M
            observatory=AU
          }

    test <@ result |>
            Option.exists ((=) expected) @>


[<Test>]
let ``parse batch`` () =

        // ARRANGE
        let time = DateTime.Parse
        let input = 
            ["2006-08-27T11:17|12,56|36|US";
            "2006-08-27T11:18|31,51|88|US";
            "2006-08-27T11:19|52,94|39|US";
            "2006-08-27T11:20|86,92|90|US";
            "2006-08-27T11:21|74,25|46|US";
            "2006-08-27T11:22|49,77|43|US";
            "2006-08-27T11:23|74,72|82|US"]

        let expected =
            [{datetime = time "2006-08-27T11:17"; location = loc(12, 56); temperature=36M; observatory = US};
            {datetime = time "2006-08-27T11:18" ; location = loc(31, 51); temperature=88M; observatory = US};
            {datetime = time "2006-08-27T11:19" ; location = loc(52, 94); temperature=39M; observatory = US};
            {datetime = time "2006-08-27T11:20" ; location = loc(86, 92); temperature=90M; observatory = US};
            {datetime = time "2006-08-27T11:21" ; location = loc(74, 25); temperature=46M; observatory = US};
            {datetime = time "2006-08-27T11:22" ; location = loc(49, 77); temperature=43M; observatory = US};
            {datetime = time "2006-08-27T11:23" ; location = loc(74, 72); temperature=82M; observatory = US}]
        
        // ACT
        let result = 
            List.map Parsing.parseLine input
            |> List.choose id

        // ASSERT
        let expectedContains x = List.contains x expected
        test <@ List.forall expectedContains result @>


[<Test>]
let ``parse batch with erroneous lines`` () =

        // ARRANGE
        let time = DateTime.Parse

        let input = 
            ["2006-08-27T11:17|12,56|36|US";
            "";
            "";
            "";
            "2006-08-27T11:21|74,25|46|US";
            "2006-08-27T11:22|49,77|43|US|2006-08-27T11:22|49,77|43|US";
            "2006-08-27T11:23|74,72|82|US"]

        let expected =
            [{datetime = time "2006-08-27T11:17"; location = loc(12, 56); temperature=36M; observatory = US};
            {datetime = time "2006-08-27T11:21" ; location = loc(74, 25); temperature=46M; observatory = US};
            {datetime = time "2006-08-27T11:23" ; location = loc(74, 72); temperature=82M; observatory = US}]
        
        // ACT
        let result = 
            List.map Parsing.parseLine input
            |> List.choose id

        // ASSERT
        let expectedContains x = List.contains x expected
        test <@ List.forall expectedContains result @>