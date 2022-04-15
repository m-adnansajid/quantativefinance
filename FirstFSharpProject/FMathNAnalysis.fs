module FMathNAnalysis


    //Numerical Types
    let fifteen = 0x402E000000000000LF

    let smallestInteger = 10uy

    let smallerInteger = 10s

    let smallInteger = 10us

    let integer = 10L

    //Arithmetic Operators
    // Note: Arithmetic Operators do net check overflow. for overflow checking use Checked module.
    let divisionOp1 = 10 % 2

    let divisionOp2 = 10 % 3

    //Arithmetic Comparison
    5.0 = 5.0
    1 < 4
    1.0 > 3.0

    //Cannot compare numbers of different types in F#
    5.0 >= 10 // false

    //Conversion Functions
    //There is no implicit conversions in F# as conversions have to be done manually using conversion routines.

    //Calculating the sum of a sequence.
    let random = new System.Random()
    let rnd() = random.NextDouble()
    let data = [for i in 1..10 -> rnd()]

    let sum = data |> Seq.sum //The result, sum, will vary from time to time due to the fact that we use a random number generator.

    //Calculating the average of a Sequence.
    let seqData = [for i in 1..500 -> rnd() * 10.0]

    let avg = seqData |> Seq.average

    //Calculating the minimum of a Sequence.
    let newDataForMin : float list = [5.0530272; 6.389536232; 6.126554094; 7.276151291; 0.9457452972; 
 7.774030933; 7.654594368; 8.517372011; 3.924642724; 6.572755164]

    let min = newDataForMin |> Seq.min

    //Calculating the maximum of a Sequence.
    let newDataForMax = [7.586052086; 22.3457242; 76.95953826; 59.31953153; 33.53864822]

    let max = newDataForMax |> Seq.max

    //Calculating variance.
    let variance values = 
        let average = Seq.average values
        values
        |> Seq.map (fun x -> (1.0 / float (Seq.length values)) * (x - average) ** 2.0)
        |> Seq.sum

    //Calculating Standard Deviation
    let stdDev1(values: seq<float>) = sqrt(variance(values))

    //using fold function.
    let stdDev2(values) = 
        let avg = Seq.average values
        values
        |> Seq.fold (fun acc x -> acc + (1.0 / float (Seq.length values)) * (x - avg) ** 2.0) 0.0
        |> sqrt 

    stdDev1 [2.0; 4.0; 4.0; 4.0; 5.0; 5.0; 7.0; 9.0]

    stdDev2 [2.0; 4.0; 4.0; 4.0; 5.0; 5.0; 7.0; 9.0];

    let var = variance data
    let std = stdDev2 data

    let avg1 = data |> Seq.average
    let sum1 = data |> Seq.sum
    let min1 = data |> Seq.min
    let max1 = data |> Seq.max
    let var1 = data |> variance
    let std1 = data |> stdDev2

    //Mersenne Twister
    open MathNet.Numerics.Random

    let mersenneTwister = new MersenneTwister(42)
    let a = mersenneTwister.NextDouble()

    //Normal Distribution
    open MathNet.Numerics.Distributions

    let normal = new Normal(0.0, 1.0)
    let mean = normal.Mean
    let variance1 = normal.Variance
    let stddev = normal.StdDev

    //Statistics
    let dist = new Normal(0.0, 1.0)
    let samples = dist.Samples() |> Seq.take 1000  |> Seq.toList

    let statistics = new MathNet.Numerics.Statistics.DescriptiveStatistics(samples)

    // Order Statistics
    let maximum = statistics.Maximum
    let minimum = statistics.Minimum

    //Central Tendency
    let mean1 = statistics.Mean

    //Dispersion
    let varianceStats = statistics.Variance
    let stdDevStats = statistics.StandardDeviation


    //Liner Regression
    /// Linear regression is heavily used in statistics where sample data is analyzed.
    ///  Linear regression tells the relationship between two variables.
    
    //Least Square Method.
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.LinearAlgebra.Double
    open MathNet.Numerics.Distributions

    /// Linear regression using least squares
    // let x = DenseMatrix.OfColumnArrays 5 2 [ List.init 5 (fun i -> 1.0); [ 10.0; 20.0; 30.0; 40.0; 50.0 ] ] x
    // let y = DenseVector [| 8.0; 21.0; 32.0; 40.0; 49.0 |]
    // let p = x.QR().Solve(y)

    // printfn "x: %A" x
    // printfn "y: %s" (y.ToString())
    // printfn "p: %s" (p.ToString())

    // let (a, b) = (p.[0], p.[1])

    // polynomial regression
    let noise = (Normal.WithMeanVariance(0.0, 0.5))
    //Sample points for x^2-3x+5
    let xdata = [-10.0 .. 0.2.. 10.0]
    let ydata = [for x in xdata do yield x ** 2 - 3.0 * x + 5.0 + noise.Sample()]

    let N = xdata.Length
    let order = 2

    //Generating a Vandermonde row given input v
    let vandermondeRow v = [for x in [0..order] do yield v** (float x)]

    /// Creating Vandermonde rows for each element in the list
    let vandermonde = xdata |> Seq.map vandermondeRow |> Seq.toList

    /// Create the A Matrix
    // let A = vandermonde |> DenseMatrix.OfColumnArrays N (order + 1)
    // A.Transpose()

    // /// Create the Y Matrix
    // let createYVector order l = [for x in [0..order] do yield l]
    // let Y = (createYVector order l ydata |> DenseMatrix.OfRowArrays (order + 1) N).Transpose()

    // /// Calculate coefficients using least squares
    // let coeff = (A.Transpose() * A).LU().Solve(A.Transpose() * Y).Column(0)

    // let calculate x = (vandermondeRow(x) |> DenseVector.OfArray) * coeff 

    // let fitxs = [(Seq.min xdata).. 0.02..(Seq.max xdata)]
    // let fitys = fitxs |> List.map calculate
    // let fits = [for x in [(Seq.min xdata) .. 0.2..(Seq.max xdata)] do yield (x, calculate x)] 

    // open FSharp.Charting
    // open System.Windows.Forms.DataVisualization.Charting

    // fsi.AddPrinter(fun (ch:FSharp.Charting.ChartTypes.GenericChart) -> ch.ShowChart(); "FSharpCharting")
    
    // let chart = Chart.Combine [Chart.Point (List.zip xdata ydata); 
    //     Chart.Line(fits).WithTitle("Polynomial regression")]

    
    ///Learning about root-finding algorithms
    /// 
    /// The bisection method.
    let rec bisect n N  (f:float->float) (a:float)(b:float)(t:float) :float =
        if n >= N then -1.0
        else
            let c = (a + b) / 2.0
            if f(c) = 0.0 || (b - a) / 2.0 < t then c //Solution foun  
            else 
                if sign(f(c)) = sign(f(a)) then 
                    bisect (n+1) N f c b t
                else 
                    bisect (n+1) N f a c t

    let f = (fun x -> (x**2.0 - x - 6.0))

    
    ///The first two arguments, 0 and 25, are used to keep track of the iterations.
    /// The next argument is the function itself that we defined in the preceding code as f.
    /// The next two arguments are limits, that is, the range within which we can look for the root.
    /// And the last one is just a value for the accuracy used for comparison inside the iteration.
    
    // First root, on the positive side
    let first = bisect 0 25 f -10.0 0.0 0.01

    // Second root, on the negative side
    let second = bisect 0 25 f -10.0 0.0 0.1

    ///Finding roots using the Newton–Raphson method
    ///The Newton-Raphson method, or simply Newton's method, usually converges 
    /// faster than the bisection method. The Newton-Raphson method also needs the 
    /// derivative of the function, which can be a problem in some cases. This is especially 
    /// true when there is no analytical solution available. 
    /// The following implementation is a modification of the bisection method using the derivative of the function to determine if a solution has been found.
    let rec newtonraphson n N (f:float->float)(fprime:float->float)(x0:float)(tol:float) : float =
        if n >= N then -1.0
        else
            let d = fprime(x0)
            let newtonX = x0 - f(x0) / d 
            if abs(d) < tol then 
                -1.0
            else
                if abs(newtonX - x0) < tol then 
                     newtonX //Solution found
                else 
                    newtonraphson ( n + 1) N f fprime newtonX tol
    ///One of the drawbacks of using the preceding method is that we use a fixed point 
    ///convergence criteria, abs(newtonX - x0) < tol, which means that we can be far 
    ///from the actual solution when this criteria is met.
    

    //Example
    let f1 = (fun x -> (x**2.0 - 2.0))
    let f1prime = (fun x -> (2.0*x))
    let sqrtOfTwo = newtonraphson 0 25 f1 f1prime 1.0 10e-10 

    ///This is the answer we would expect, and the method works for finding roots! Notice 
    ///that if we change the starting value, x0, from 1.0 to -1.0, we'll get the negative root:
    newtonraphson 0 25 f f1prime -1.0 10e-10
    /// This is also a valid solution to the equation, so be aware of this when you use this 
    ///method for solving the roots. It can be helpful to plot the function, as we did in the 
    ///section about the bisection method, to get a grip on where to start from.
    
    /// TODO: Rewrite and fix bugs.
    ///Finding roots using the secant method
    // let rec secant n N (f:float -> float) (x0:float) (x1:float) (x2:float) :float =
    //     if n >= N then x0
    //     else
    //         let x = x1 - (f(x1))*((x1 - x0)/(f(x1) - f(x0)))
    //         secant (n + 1) N f x x0
    
    // //Example
    // let f = (fun x -> (x**2.0 - 612.0))

    // secant 0 10 f 0.0 10.0 30.0 
    


