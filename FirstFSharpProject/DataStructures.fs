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
    ///Tail recursion is a way of optimizing the recursion and easing the callback stack