(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**
Statistics on Series and Data Frames
================
*)

open System
open System.IO
open FSharp.Data
open Deedle
open MathNet.Numerics

//matrix algebra 101

open MathNet.Numerics.LinearAlgebra
let m = matrix [[ 1.0; 2.0 ]
                [ 3.0; 4.0 ]]
let m' = m.Inverse()
let ev = m.Evd()
let eigenvals = ev.EigenValues

//try with an actual matrix - this is GBP MC SVJD only
let actualM = matrix [  [ 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
                        [ 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
                        [ 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
                        [ 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
                        [ 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.562708685; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; -4.93328702048261E-02; -0.1 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.576466814; 0.0; 0.0; 0.0; 0.0; 0.0; -0.123541386456077; -0.04 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.562708685; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; -4.24698933381981E-02; -0.1 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.576466814; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; -0.213401672200869; -0.1 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; -4.93328702048261E-02; -0.123541386456077; -4.24698933381981E-02; -0.213401672200869; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.39 ]
                        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; -0.1; -0.04; -0.1; -0.1; 0.0; 0.0; 0.0; 0.0; 0.0; 0.39; 1.0 ]]

let actualDet =  actualM.Determinant() //if this exists, then it must be invertable, hence PSD
let actualEv = actualM.Evd()
let actualEigenvals  = actualEv.EigenValues
let minEigenVal = actualEigenvals.Real() |> Seq.min //eigenvalues have been calculated as complex numbers, so we need to take the real part
let isPSD = minEigenVal >= 0.000001 //larger than zero, with some buffer

let oneLineIsPSD = actualM.Evd().EigenValues.Real() |> Seq.min >= 0.000001

//
let root = __SOURCE_DIRECTORY__ + "/data/"

// load a test dataset with intentionally missing values
let air = Frame.ReadCsv(root + "AirQuality.csv", separators=";")
let ozone = air?Ozone

// example stats on a series
series [
  "Mean" => round (Stats.mean ozone)
  "Max" => Option.get (Stats.max ozone)
  "Min" => Option.get (Stats.min ozone)
  "Median" => Stats.median ozone ]

// can call stat functions across an entire frame
let info = 
  [ "Min" => Stats.min air
    "Max" => Stats.max air
    "Mean" => Stats.mean air
    "+/-" => Stats.stdDev air ] |> frame



// Multivariate statistics
//open MathNet.Numerics
open MathNet.Numerics.Statistics;

let myX = [ 1.5; 3.5; 2.5; 8.5; 10.5; ]
let myY = [ 2.5; 2.5; 4.5; 7.5; 11.5; ]
let myZ = [ 13.3; 8.9; 5.0; 4.2; 3.1; ]

Statistics.Maximum myX
Statistics.PopulationStandardDeviation myX

let myCorrelXY = Statistics.Correlation.Pearson(myX, myY) //=0.941298884207501, according to excel
let myCorrelXZ = Statistics.Correlation.Pearson(myX, myZ) //=-0.767560631901349, according to excel

//open MathNet.Numerics.LinearAlgebra
//let myMat = matrix [ myX
//                     myY
//                     myZ ]

//Statistics.Correlation.PearsonMatrix myMat

// Moving window statistics
ozone |> Stats.movingMean 3

// moving functions mark the first n-1 key values as missing
// the min/max functions work differently, and show the value over the smaller window

ozone |> Stats.movingMin 3

// EXPANDING WINDOWS

// Expanding on a time series - Stats over all observations to date
let exp = [ 
  "Ozone" => ozone
  "Mean" => Stats.expandingMean(ozone)
  "+/-" => Stats.expandingStdDev(ozone) ] |> frame

// MULTI-LEVEL INDEXED STATISTICS

// combine our test data into one with a multi-level key
open System.Globalization

let dateFormat = CultureInfo.CurrentCulture.DateTimeFormat

let byMonth = air |> Frame.indexRowsUsing (fun r ->
  dateFormat.GetMonthName(r.GetAs("Month")), r.GetAs<int>("Day"))

// we can now calculate level stats over the first level (months)
byMonth?Ozone |> Stats.levelMean fst

// Some work need to apply to the whole data frame
byMonth
|> Frame.sliceCols ["Ozone";"Solar.R";"Wind";"Temp"]
|> Frame.getNumericCols
|> Series.mapValues (Stats.levelMean fst)
|> Frame.ofRows

(**
R interoperablity
================
*)

// we need the packages (part of FsLab, so we have these referenced already here...

(**
#load "RProvider.fsx"
#load "Deedle.fsx"
*)

open RProvider
open RDotNet
//open Deedle

open RProvider.datasets

// Get from R into Deedle

// Get mtcars as an untyped object
R.mtcars.Value

// Get mtcars as an untyped object
let mtcars : Frame<string, string> = R.mtcars.GetValue()

//#load "FSharp.Charting.fsx"
open FSharp.Charting

mtcars
|> Frame.groupRowsByInt "gear"
|> Frame.getCol "mpg"
|> Stats.levelMean fst
|> Series.observations |> Chart.Column

// Push from Deedle to R

// let's get some data (we already have this above)
//let air = Frame.ReadCsv(root + "AirQuality.csv", separators=";")

open RProvider.``base``

// pass data into R and print the R output
R.as_data_frame(air)

// Pass data to R and get column means (note that R does not handle the missing values automatically)
R.colMeans(air)

// Create sample data frame with missing values
let df = 
  [ "Floats" =?> series [ 1 => 10.0; 2 => nan; 4 => 15.0] 
    "Names" =?> series [ 1 => "one"; 3 => "three"; 4 => "four"] ]
  |> frame 

// here's how R handles missing values in numeric (NaN) and non-numeric (<NA>) columns
R.assign("x", df)

// TIME SERIES DATA

// Deedle uses the "Zoo" package

// load the "austres" data set from R (number of Austrailian residents)
R.austres.Value

// Get series with numbers of australian residents
let austres : Series<float, float> = R.austres.GetValue()

// Get TimeSpan representing (roughly..) two years
let twoYears = TimeSpan.FromDays(2.0 * 365.0)
// Calculate means of sliding windows of 2 year size 
austres
|> Series.mapKeys (fun y ->
    DateTime(int y, 1 + int (12.0 * (y - floor y)), 1))
|> Series.windowDistInto twoYears Stats.mean

// deedle only supports time series with a single column, so we need to extract columns we are interested in
let ftseStr = R.parse(text="""EuStockMarkets[,"FTSE"]""")
let ftse : Series<float, float> = R.eval(ftseStr).GetValue()

// From Deedle to R

// some example data
let rnd = Random()
let ts = [ 
  for i in 0.0 .. 100.0 -> 
    DateTime.Today.AddHours(i), rnd.NextDouble() ] |> series

// we will use the "zoo" library in R to handle time series
open RProvider.zoo

// Just convert time series to R
R.as_zoo(ts)

// Convert and assing to a variable 'ts'
R.assign("ts", ts)

// Assignment not usually neccessary - can call the R functions directly

// Rolling mean with window size 20
R.rollmean(ts, 20)

// Use 'rollmean' to calculate mean and 'GetValue' to 
// turn the result into a Deedle time series

let tf = 
  [ "Input" => ts
    "Means5" => R.rollmean(ts, 5).GetValue<Series<_,float>>()
    "Means10" => R.rollmean(ts, 10).GetValue<Series<_,float>>() ]
  |> frame

// Chart original input and the two rolling means
Chart.Combine
  [ Chart.Line(Series.observations tf?Input)
    Chart.Line(Series.observations tf?Means5)
    Chart.Line(Series.observations tf?Means10) ]

// My test bed from here on in
open RProvider.stats

let ts1 = [ 
  for i in 0.0 .. 100.0 -> 
    DateTime.Today.AddHours(i), rnd.NextDouble() ] |> series
let ts2 = [ 
  for i in 0.0 .. 100.0 -> 
    DateTime.Today.AddHours(i), rnd.NextDouble() ] |> series
let ts3 = [ 
  for i in 0.0 .. 100.0 -> 
    DateTime.Today.AddHours(i), rnd.NextDouble() ] |> series

let tsf = 
  [ "One" => ts1
    "Two" => ts2
    "Three" => ts3
    "OneAgain" => ts1 ]
  |> frame

R.cor(tsf)
R.cov(tsf)

//TODO: Get some TR data, and bind it into a frame?
