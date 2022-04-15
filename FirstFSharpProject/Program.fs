

// For more information see https://aka.ms/fsharp-console-apps

open System.Windows.Forms
[<EntryPoint>]
let main argv = 
    printfn "%A" argv


    let form = new Form (Text = "F# Events", Visible = true, TopMost= true)
    form.Click.Add(fun evArgs -> System.Console.WriteLine("Click even handler"))
    Application.Run(form)

    form.MouseDown
    |> Event.filter (fun args -> args.X < 50)
    |> Event.map (fun args -> printfn "%d %d" args.X args.Y)

    0 //return an integer exit code 
