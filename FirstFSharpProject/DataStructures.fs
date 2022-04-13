module DataStructure

    //modeling an open-high-low-close (OHLC) bar using record types
    type OHLC = {
        o: float
        h: float
        l: float
        c: float
    }

    let ohlcBar : OHLC = {
        o = 1.0 
        h = 2.0
        l = 3.0
        c = 4.0
    }

    type Quote = 
        {
            mutable bid: float
            mutable ask: float
        } 
        member this.midpoint() = (this.bid + this.ask) /2.0

    let q: Quote = { bid = 100.0; ask = 200}

    q.bid <- 150.0
    q.midpoint()


    let matchQuote (quote: Quote) =
        match quote with
        | {bid = 0.0; ask = 0.0} -> printfn "Both bid and ask is zero" 
        | {bid = b; ask = a} -> printfn "bid: %f, ask: %f" b a

    let quote1: Quote = {bid = 200.0; ask = 100.0}
    let quote2: Quote = {bid = 0.0; ask = 0.0}

    matchQuote quote1
    matchQuote quote2


    //Discriminated Unions
    //represent heterogeneous data and support data that can be a set of named cases.
    //Discriminated unions represent a finite, well-defined set of choices.

    type OrderType = 
        | Buy
        | Sell
        | Market
        | Limit

    let buy = Buy
    let sell = Sell

    
    let toggle1 x = 
        match x with 
        | Buy -> Sell
        | Sell -> Buy

    
    let toggle2 = 
        function
        | Sell -> Buy
        | Buy -> Sell

    let testOrderSide() = 
        printfn "Buy: %A" buy
        printfn "Sell: %A" sell
        printfn "Toggle Buy: %A" (toggle1 buy)
        printfn "Toggle Sell: %A" (toggle2 sell)
        
    testOrderSide() 



    //Recursive Fields.
    type OptionT = 
        | Put of float
        | Call of float
        | Combine of OptionT * OptionT

    //Another Example
    type Tree =
        | Leaf of int
        | Node of Tree * Tree

    let SimpleTree = Node (Leaf 1, Leaf 2)

    //Iterate Tree
    let countLeaves tree =
        let rec loop sum = function
            | Leaf (_) -> sum + 1
            | Node (tree1, tree2) -> sum + (loop 0 tree1) + (loop 0 tree2)
        loop 0 tree
    
    //Enumerations: are used to map labels to constant values.
    type RGB =
        | Red = 0
        | Green = 1
        | Blue = 2

    let col1: RGB = RGB.Red

    //Arrays: Arrays are mutable collections of a fixed size and must contain values of the same type.
    let array1 = [| 1; 2; 3 |]

    array1.[0] <- 10

    //All elements in an array have to be of the same type.
    let array2 = [| 1.0; 2.3 |]

    let array3 = [| for i in 1 .. 10 -> i * i |]

    //Accessing elements
    array1.[1]

    //slice notation.
    array1.[0..2]

    //accessing from the beginning.
    array1.[..2]

    //reverse is the same
    array1.[2..]

    //array initialization.
    let arrayOfTenZeros : int array = Array.zeroCreate 10 

    //create a totally empty array
    let myEmptyArray = Array.empty

    printfn "%A" (Array.append [| 1; 2; 3 |] [| 4; 5; 6 |])

    printfn "%A" (Array.filter (fun elem -> elem % 2 = 0) [| 1 .. 10 |])

    //Lists.
    let list1 = [1..10]

    let list2 = [ for i in 1..10 -> i * i]

    10 :: [10]

    10 :: list1

    [10] @ list1

    list1 @ list2

    List.nth list1 3

    let list3 : float list = [10.0; 11.0; 12.0; 13.0; 14.0; 15.0; 16.0; 17.0; 18.0; 19.0; 20.0]

    List.average list3

    //List.average list1 //error

    List.min list1

    List.max list1

    List.append list1 list2

    List.filter (fun elem -> elem > 10) list1

    List.filter (fun elem -> elem > 3) list1

    List.find (fun elem -> elem > 3) list1

    List.exists (fun elem -> elem > 3) list1

    List.exists (fun elem -> elem > 10) list1

    //The zip function will create a new list using a pair-wise combination of the elements found in lists provided as arguments.
    List.zip list1 list2


    List.fold (+) 0 list1

    List.fold (*) 1 list1

    //Pattern matching and Lists.
    
    //Get the lenght of a list.
    let rec getLenghtOfList l = function
        | [] -> printfn "Lenght of list: %d" l 
        | head :: tail -> getLenghtOfList (l+1) tail

    let myList = [1..10]
    getLenghtOfList 0 myList

    //Get the second last element of list.
    let rec getSecondLastElement = function
        | head :: tail :: [] -> head
        | head :: tail -> getSecondLastElement tail
    
    getSecondLastElement myList/// Because a list is constructed using the cons operator (::), 
                            ///we can match arbitrary patterns on lists using it. The first pattern will match head :: 
                            ///tail :: [], where head is the second last element in the list.
    

    //Sequences...
    seq{ 1..2 }

    let seq1 = seq{ 1..10 }

    seq{ 1..10..100 }

    seq{for i in 1..10 do yield i * i }

    seq{ for i in 1..10 -> i * i }

    Seq.item 3 {1..10}

    Seq.average {0.0 .. 100.0}

    //Seq.min seq{1..10}

    //Seq.max

    Seq.empty 

    Seq.filter (fun elem -> elem > 3) seq1

    Seq.find (fun elem -> elem > 3) seq1

    Seq.exists (fun elem -> elem > 3) seq1

    Seq.exists (fun elem -> elem > 10) seq1

    Seq.head seq1 //Seq.tail does not exist because sequences are lazy constructs...

    //SETS...
    let set1 = set[1; 2; 7]

    set1.Add(9)

    //To check sets are immutable.
    set1.Contains 1 
    Set.contains 1 set1
    set1.Contains 9
    Set.contains 9 set1

    //create set from other data structures in this case from sequence.
    let set2 = Set.ofSeq [1..10]
    
    //It works the same way as creating it from an array.
    let set3 = Set.ofArray [| for i in 1 .. 5 -> i * i |]

    Set.count set1

    //The fold function is also present in the module for sets and is used in the same way as for other collections.
    Set.fold (fun a b -> a + b) 0 set1

    Set.fold (fun a b -> a * b) 1 set1

    //This can also be written using the shorthand version (+) for addition and (*)for multiplication
    Set.fold (+) 0 set1
    Set.fold (*) 1 set1

    Set.exists (fun elem -> elem = 2) set1
    Set.exists ((=) 4) set1

    Set.filter (fun elem -> elem > 1) set1
    Set.filter (fun elem -> elem < 2 ) set1

    //partition function: will split the set, in this case, into two new sets: a set of elements which passes the predicate and one that doesn't
    Set.partition (fun elem -> elem < 2) set1

    Set.map (fun elem -> elem + 2) set1

    //MAPS...
    /// Maps are a special kind of set with associative key/value pairs.
    /// They are immutable, unordered data structures
    /// They do not preserve the order of elements as they are inserted, nor do they permit duplicates
    let map1 = Map.empty.Add("Age",27)

    map1.["Age"]

    //It's possible to create maps from lists of values.
    let map2 = ["Year", 2009; "Month", 21; "Day", 3] |> Map.ofList

    Map.filter (fun _ v -> v = 27) map1

    Map.exists (fun _ v -> v = 27) map1

    Map.partition (fun _ v -> v = 27) map1

    Map.containsKey "Age" map1
    Map.containsKey "Ages" map1

    //Options
    ///Options are an elegant way of enclosing a value that may or may not exist. 
    /// They are implemented using a discriminated union. 
    /// Instead of checking for null values, options are preferred
    let evalOption (o: int option) =
        match o with
        | Some (a) -> printfn "Found value: %d" a 
        | None -> printfn "None"
    
    let some : int option = Some(1)
    let none : int option = None

    evalOption some
    evalOption none

    //Strings...
    let str1 = "This is a string"

    let srt2 = @"This is a string with \ \ / /"

    let str3 = """ this is "another" string"""

    printfn "%s" (str1.[0..2])

    let str4 = "Hello, " + "world"

    let str5 = str1 + str3

    //String.Compare (str1, "This is a string")
    //String.Compare (str1, "This is another string")

    //String.map (fun s -> Char.ToUpper s) str1

    
    
    //Recursive Functions...
    let rec fib n =
        if n <= 2 then 1
        else fib (n - 1) + fib (n - 2)

    let rec sum list = 
        match list with 
        | head :: tail -> head + sum tail
        | [] -> 0
    
    let rec len list =
        match list with
        | head :: tail -> 1 + len tail
        | [] -> 0
    
    let rec mymap f = function 
        | [] -> []
        | x::xs -> f x :: mymap f xs

    //Tail Recursion...
    ///Tail recursion is a way of optimizing the recursion and easing the callback stack.
    ///An optimization technique.
    /// A way to ease the stack and ensure there an no stack overflows.
    /// Sometimes harder to understand and reason about.
    let rec factorial1 n = 
        match n with 
        | 0 | 1 -> 1
        | _ -> n * factorial1(n - 1)
    
    let factorial2 n =
        let rec tailrecfact n acc = 
            match n with
            | 0 -> acc
            | _ -> tailrecfact (n - 1) (acc * n)
        tailrecfact n 1

    //Pattern Matching.
    ///Pattern matching is used for control flow.
    ///It allows programmers to look at a value, test it against a series of conditions, 
    /// and perform certain computations depending on whether that condition is met
    let sampleMatcher value =
        match value with 
        | 0 -> "Zero"
        | 1 -> "One"
        | _ -> "Greater than One"
    
    sampleMatcher 0

    //Incomplete Pattern Matching.
    let incompleteSampleMatcher value = 
        match value with 
        | 0 -> "Zero"
        | 1 -> "One"    

    let nameMatcher name =  
        match name with
        | "John" -> "The name is John"
        | "Bob" -> "Hi Bob!"

    //Fixed using wildcard operator (_)
    let nameMatcherFixed name =
        match name with
        | "John" -> "The name is John"
        | "Bob" -> "Hi Bob!"
        | _ -> "I don't know you!"
    
    nameMatcherFixed "John"
    nameMatcherFixed "Linda"

    //Guards...
    ///In imperative programming, we use if statements with expressions to express conditions.
    /// This is accomplished using pattern matching and guards.
    /// Guards use the keyword when to specify the condition.
    let sampleMatcherWithGuard value = 
        match value with
        | 0 -> "Zero"
        | 1 -> "One"
        | x when x > 1 -> "Greater than one"
        | _ -> "Some strange value"

    //The name guard tells us something about their properties. They guard the pattern based on conditions.



    //Pattern matching in assignment and input parameters...
    let (bid, ask) = (100, 110.0)

    let (x, y, z) = (3.0, 2.0, 4.0)

    let (a, b, _) = (3.0, 2.0, 4.0)

    //Active Patterns
    ///Active patterns allow programmers to wrap ad hoc values and objects in union-like structures for use in pattern matching.
    let (|Negative| Positive|) number = 
        if number >= 0.0 then 
            Positive
        else
            Negative
    
    let TestNumber (number:float) = 
        match number with 
        | Positive -> printfn "%f is positive " number
        | Negative -> printfn "%f is negative " number 

    // let (|Limit|Market|Invalid|) (order: Order) =
    //     if order.Type = OrderType.Limit && order.Price > 0.0 then 
    //         Limit
    //     else if order.Type = OrderType.Market && order.Price = 0.0 then 
    //         Market 
    //     else 
    //         Invalid
    
    // let TestOrder (order:Order) = 
    //     match order with 
    //     | Market -> printfn "Market order"
    //     | Limit -> printfn "Limit order"
    //     | Invalid -> printfn "Invalid order"

    
    // TestOrder (Order(Buy, Limit, 5.0))
    // TestOrder (Order(Sell, Market, 0.0))
    // TestOrder (Order(Sell, Market, 2.0))
    // TestOrder (Order(Sell, Invalid, 5.0))


    ///Partial Active Patterns are used when parts of the input match,
    /// which is helpful in the case where strings need to be parsed to numbers.
    let(|Integer|_|) (str:string) = 
        match System.Int32.TryParse(str) with
        | (true, num) -> Some (num)
        | _ -> None

    let (|Double|_|) (str:string) = 
        match System.Double.TryParse(str) with
        | (true, num) -> Some (num)
        | _ -> None

    let testParse numberStr = 
        match numberStr with 
        | Integer num -> printfn "Parsed an integer '%A'" num
        | Double num -> printfn "Parsed a double '%A'" num
        | _ -> printfn "Couldn't parse string: %A" numberStr

    //TODO: Need to be redefined.
    //Generics..
    // let genericListMaker<'T> (x:'T,y:'T,z:'T) = 
    //     let list = new List<'T>()
    //     list.Add(x)
    //     list.Add(y)
    //     list.Add(z)
    //     list

    // genericListMaker<int> (1,2,3)
    
    // genericListMaker<float>(1.0, 2.0, 3.0)

    // genericListMaker<string>("1", "2", "3")

    //Lazy Evaluation.
    let lazyListFolding = 
        lazy( 
            let someList = [for i in 1..10 -> i *2]
            List.fold (+) 0 someList
            )

    //Units of Measure.
    ///Are a way to associate signed integers and floating point numbers with units.
    /// The units can describe weight, length, volume and currencies.
    
    // Conversion rate representing 1 EUR in USD.
    let rateEurUsd = 1.28M

    //Converts amount in EUR to USD.
    let euroToUsds eur = eur * rateEurUsd

    //Convert 1000 EUR to UDS.
    //let usd = euroToUsds 10000.0M

    [<Measure>]
    type USD

    [<Measure>]
    type EUR

    let rateEuroUsd = 1.28M<EUR/USD>

    //converts amount in EUR to USD.
    let eurToUsds (eur: decimal<EUR>) = eur * rateEuroUsd

    //convert 10000 EUR to USD.
    let usd = eurToUsds 10000.0M<EUR>

    [<Measure>]
    type YEN

    //Convert 10000 EUR to USD
   // let usd = eurToUsds 10000.0M<YEN> //TypeMismatch Error.

   ///Asynchronous and Parallel Programming.
   /// Events...are useful when you want a certain function to execute upon a specific event that will occur sometime in the future.
    // open System.Windows.Forms

    // let form = new Form (Text = "F# Events", Visible = true, TopMost= true)
    // form.Click.Add(fun evArgs -> System.Console.WriteLine("Click even handler"))
    // Application.Run(form)

    // form.MouseDown
    // |> Event.filter (fun args -> args.X < 50)
    // |> Event.map (fun args -> printfn "%d %d" args.X args.Y)


    ///Background workers...
    /// Background workers are a solution when you want to execute long running tasks that run in the background.
    /// The code will be executed in a separate thread:
    open System.Threading
    open System.ComponentModel

    let worker = new BackgroundWorker()
    worker.DoWork.Add(fun args ->
        for i in 1..50 do 
            //Simulates heavy calculation
            Thread.Sleep(1000)
            printfn "%A" i
    )

    worker.DoWork.Add(fun args -> 
        for i in 1 .. 10 do 
            //Simulates heavy calculation
            Thread.Sleep(500)
            printfn "B: %A" i 
    )

    worker.RunWorkerCompleted.Add(fun args -> printfn "Completed...")

    worker.RunWorkerAsync()

    //To be able to cancel the execution in the Backgroundworker.
    open System.ComponentModel

    let workerCancel = new BackgroundWorker(WorkerSupportsCancellation = true)
    workerCancel.DoWork.Add(fun args ->
        printfn "apan %A" args
        for i in 1..50 do
            if (workerCancel.CancellationPending = false) then 
                Thread.Sleep(1000)
                printfn "%A" i
    )

    workerCancel.RunWorkerCompleted.Add(fun args -> printfn "Completed...")
    workerCancel.RunWorkerAsync()

    //To do a cancellation on the preceeding execution, you need to run the follwoing line:
    workerCancel.CancelAsync()

    //Threads...
    let runMe() = 
        for i in 1..10 do 
            try 
                Thread.Sleep(1000)
            with
                |   :? System.Threading.ThreadAbortException as ex -> printfn "Exception %A" ex
            printfn "I'm still running..."

    let thread = new Thread(runMe)
    thread.Start()

    //We spawn thread by passing a delegate to the Thread constructor.
    let createThread() = 
        let thread = new Thread(runMe)
        thread.Start()

    createThread()
    createThread()

    //Thread Pools: create and execute a task.
    let runMeThread(arg:obj) = 
        for i in 1..10 do
            try
                Thread.Sleep (1000)
            with
                |   :? System.Threading.ThreadAbortException as ex -> printfn "Exception %A" ex
            printfn "%A still running..." arg

    // Enqueue three tasks to be executed by the thread pool.
    //They will all be executed without being enqueued. 
    //This is because the thread pool will spawn up to the ThreadPool.GetMaxThreads() threads, which are usually 1024 threads.
    ThreadPool.QueueUserWorkItem(new WaitCallback(runMeThread), "One")
    ThreadPool.QueueUserWorkItem(new WaitCallback(runMeThread), "Two")
    ThreadPool.QueueUserWorkItem(new WaitCallback(runMeThread), "Three")

    //Asynchronous Programming...
    ///Asynchronous code performs requests that are not completed immediately. 
    ///That means they are doing operations that will be completed sometime in the future without blocking the current thread
    open Microsoft.FSharp.Control.WebExtensions

    //Stock symbol and URL yahoo finance.
    let urlList =  [ "MSFT", "http://ichart.finance.yahoo.com/table.csv?s=MSFT&d=6&e=6&f=2013&g=d&a=1&b=1&c=2010&ignore=.csv"; 
                        "GOOG", "http://ichart.finance.yahoo.com/table.csv?s=GOOG&d=6&e=6&f=2013&g=d&a=1&b=1&c=2010&ignore=.csv";
                        "EBAY", "http://ichart.finance.yahoo.com/table.csv?s=EBAY&d=6&e=6&f=2013&g=d&a=1&b=1&c=2010&ignore=.csv";
                        "AAPL", "http://ichart.finance.yahoo.com/table.csv?s=AAPL&d=6&e=6&f=2013&g=d&a=1&b=1&c=2010&ignore=.csv";
                        "ADBE", "http://ichart.finance.yahoo.com/table.csv?s=ADBE&d=6&e=6&f=2013&g=d&a=1&b=1&c=2010&ignore=.csv"
                    ]

    /// Parse CSV and extract max price
    /// /// <summary>Extracts the maximum closing price from the provided CSV string</summary>
    ///<param name="str">Unparsed CSV string.</param>
    ///<remarks>Will leave the two last lines unhandled, due to Yahoo specific conditions</remarks>
    ///<returns>The maximum closing price for the entire sequence.</returns>
    let getMaxPrice (data:string) =
        let rows = data.Split('\n')
        rows
        |> Seq.skip 1
        |> Seq.map (fun s -> s.Split(','))
        |> Seq.map (fun s -> float s.[4])
        |> Seq.take (rows.Length - 2)
        |> Seq.max

    ///Let Async fetch of CSV data.
    let fetchAsync(name, url:string)=
        async{
            try 
                let uri = new System.Uri(url)
                let webClient = new WebClient()
                let! html = webClient.AsyncDownloadString(uri)
                let maxPrice = (getMaxPrice(html.ToString()))
                printfn "Downloaded historical data for %s, max closing price since 2010-01-01: %f" name maxPrice
            with
            | ex -> printfn "Exception: %s" ex.Message
        }

    /// Helper function to run in async parallel
    let runAll() = 
        urlList
        |> Seq.map fetchAsync
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

    /// Get max closing price from 2010-01-01 for each stock
    runAll()    


    ///Parallel programming using TPL.
    /// 
    /// Mailbox Processor: also called an agent.
    /// Agents are constructs to support concurrent applications without too many insights into the implementation details. 
    /// They are also more suitable where no shared memory is used, for example, in distributed computer systems with many nodes.
    
    //Type of our Agent:redefine MailboxProcessor to be called Agent.
    type Agent<'T> = MailboxProcessor<'T>

    //Control messages to be sent to agent.
    type CounterMessage =
        | Update of float
        | Reset 
    
    module Helpers = 
        let getRandomNumber (n) =
            let rnd = new System.Random(n)
            float (rnd.Next(n,100))
    ///the module MaxAgent is defined with the Agent itself.
    ///  The Agent reacts upon received messages using pattern matching
    module MaxAgent = 
        // Agent to keep track of max value and update GUI.
        let sampleAgent = Agent.Start(fun inbox -> 
            let rec loop max = 
                async {
                    let! msg = inbox.Receive()
                    match msg with
                    | Reset -> return! loop 0.0
                    | Update value -> 
                        let max = System.Math.Max(max,value)
                        System.Console.WriteLine("Max: " + max.ToString())

                        do! Async.Sleep (1000)
                        return! loop max 
            }
            loop 0.0)


    let agent = MaxAgent.sampleAgent
    let random = Helpers.getRandomNumber 5
    agent.Post(Update random)

    //To reset.
    agent.Post(Reset)

    //Send some updates with random values:
    let random = Helpers.getRandomNumber 5
    agent.Post(Update random)
    //Agents are powerful constructs that act as state machines.
    // You send messages to them and they change state depending on the logic they contain.


    ///Object-oriented programming 
    // /// Classes
    type OrderSide = 
        | Buy
        | Sell
     
    type Order(s: OrderSide, t: OrderType, p: float) =
        member this.S = s 
        member this.T = t 
        member this.P = p

    let order = Order(Buy, Limit, 45.50)

    order.S
    order.T
    order.P
    /// 
    /// Objects and Members
     // Toggle order side
    type OrderClass(s: OrderSide, t: OrderType, p: float) =
        let mutable S = s
        member this.T = t
        member this.P = p
        member this.Side
            with get() = S
            and set(s) = S <- s
        member this.Type 
            with get() = this.T
        member this.Price 
            with get() = this.P
        member this.toggleOrderSide() =
            match S with
            | Buy -> S <- Sell
            | Sell -> S <- Buy
        
        
        member private this.toggleOrderSide(s: OrderSide) =
            match s with
            | Buy -> Sell
            | Sell -> Buy

        /// Overloaded Operators: Overloaded operators can be useful if a specific functionality has to be exposed without calling a particular function.
        static member (~-) (o: OrderClass) =
            OrderClass(o.toggleOrderSide (o.Side), o.Type, o.Price) 
    
    let order = OrderClass(Buy, Limit, 45.50)
    order.Side
    order.toggleOrderSide()
    order.Side
    order.toggleOrderSide()
    order.Side


    /// Methods and Properties
    order.Type
    order.Price

    let order1 = OrderClass(Buy, Limit, 50.00)

    let order2 = -order1


    