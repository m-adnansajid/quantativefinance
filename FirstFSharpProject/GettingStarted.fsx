printfn "Hello, World from F#"

let sum = 4 + 5

let newsum = sum + 3

let immutable = "I am immutable!"

//immutable <- "Try to change it..."

let firstname = "John"
let lastname = "Doe"
let mutable name = firstname + lastname
name <- "John Johnson"

/// Open the System.IO namespace
open System.IO
/// Sample stock data, from Yahoo Finance
let stockData = [
 "2013-06-06,51.15,51.66,50.83,51.52,9848400,51.52";
 "2013-06-05,52.57,52.68,50.91,51.36,14462900,51.36";
 "2013-06-04,53.74,53.75,52.22,52.59,10614700,52.59";
 "2013-06-03,53.86,53.89,52.40,53.41,13127900,53.41";
 "2013-05-31,54.70,54.91,53.99,54.10,12809700,54.10";
 "2013-05-30,55.01,55.69,54.96,55.10,8751200,55.10";
 "2013-05-29,55.15,55.40,54.53,55.05,8693700,55.05"
]
/// Open the System.IO namespace
open System.IO
let filePath = @" table.csv"
/// Split row on commas
let splitCommas (l:string) =
 l.Split(',')
/// Read a file into a string array
let openFile (name : string) =
    try
        let content = File.ReadAllLines(name)
        content |> Array.toList
    with
        | :? System.IO.FileNotFoundException as e -> printfn "Exception! %s " e.Message; ["empty"]

/// Get the row with lowest trading volume, from file
let lowestVolume =
    openFile filePath
    |> List.map splitCommas
    |> Seq.skip 1
    |> Seq.minBy (fun x -> (int x.[5]))

/// Use printfn with generic formatter, %A
printfn "Lowest volume, found in row: %A" lowestVolume

/// Reverses the price data from the CSV-file
let reversePrices =
    openFile filePath
    |> List.map splitCommas
    |> List.rev

let desiredLowestValue = lowestVolume.[0]

let dateTime = System.DateTime.ParseExact(lowestVolume.[0], "yyyy-mm-dd", null)