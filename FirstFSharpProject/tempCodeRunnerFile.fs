
    let form = new Form (Text = "F# Events", Visible = true, TopMost= true)
    form.Click.Add(fun evArgs -> System.Console.WriteLine("Click even handler"))
    Application.Run(form)

    form.MouseDown
    |> Event.filter (fun args -> args.X < 50)
    |> Event.map (fun args -> printfn "%d %d" args.X args.Y)