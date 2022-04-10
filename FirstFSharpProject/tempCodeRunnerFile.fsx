let lowestVolume =
    stockData
    |> List.map splitCommas
    |> List.minBy (fun x -> (int x.[5]))