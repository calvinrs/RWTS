(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**
FsLab Experiment
================

We will require references to:
*)
open System
open System.IO
open FSharp.Data
open Deedle

(**
In this example, I will pull the time series data from CSV files I have prepared.
These are created from Excel, so will have 15dp precision - so a small loss compared to SQL

We want to match the spreadsheet version of the UncVol calculations that can be found in U:\CS\2016 Projects\Draft Ideas\ModelHistoryQueries\RWTS_RealDataAnalysis_UncVol.xlsm
*)

(** First, some constants we will use *)

let lambda = 0.98
let init = 0.031102706532478
let calibration_date = DateTime(2016, 6, 30)

let root = __SOURCE_DIRECTORY__ + "/data/"


(** We can read in the TRI series into a Deedle Data frame
We specify the "Date" column as the key, which takes the type <DateTime> *)
let TRI = 
  Frame.ReadCsv<DateTime>(root + "TSExampleData/TR_E_CNY.csv", indexCol="Date")
  |> Frame.sortRowsByKey

(*** include-value: TRI ***)

(** Same deal with short rates *)
let Rates3M = 
  Frame.ReadCsv<DateTime>(root + "TSExampleData/3mRates_CNY.csv", indexCol="Date")
  |> Frame.sortRowsByKey

// extract the TRI data and convert to a numerical type
let TRIVals = TRI.GetColumn<float>("TimeSeries.TotalReturnIndex")

// Calculate log changes over 3M in TRI, and drop the first 3m where we cannot compare
let LogTRQtrly = log(TRIVals) - log(TRIVals.Shift(3)) |> Series.dropMissing

// get the 3M rates, that we want to be in XS of
// rates are as a PCt, so divide by 100
// convert to a continuously compounded 3M rate
// TODO - Fill missing with CPI changes, for the economies that have missing short rates
let QtrlyRate = 0.25 * log(1 + Rates3M.Shift(3)/100.0)
let QtrlyRateSeries = QtrlyRate.GetColumn<float>("TimeSeries.3MRates")

// Calculate the XS return over the 3m rate - when there are no rates, use the plain return
let XSReturn = LogTRQtrly - QtrlyRateSeries |> Series.fillMissingUsing (fun k -> LogTRQtrly.Get(k))

// Return values for each quarter end
let QlyXSReturn = XSReturn |> Series.filter (fun k v -> k.Month % 3 = 0)


// We Calculate the variance, starting with the "init" value
let initDate = XSReturn.GetKeyAt(0)
let initPoint = Series([initDate], [init])

let variance = 
    QlyXSReturn 
    |> Series.filter (fun k v -> k.Equals(initDate) = false) 
    |> Series.scanValues (fun var ret -> lambda * var + (1.0 - lambda) * ret ** 2.0 ) init
    |> Series.merge initPoint


// create a frame to merge the Qly results with the monthly inputs
let df = [ "XSReturns" => XSReturn; "QlyVariance"  => variance ] |> Frame.ofColumns

let interpVarWithMissing = df?QlyVariance
// Fill missing values using interpolation function
let interpVar = interpVarWithMissing |> Series.fillMissingUsing (fun k ->
  // Get previous and next values, and the monthly scaling factor between points
  let prev = interpVarWithMissing.TryGet(k, Lookup.ExactOrSmaller)
  let next = interpVarWithMissing.TryGet(k, Lookup.ExactOrGreater)
  let monthlyScale = float((k.Month % 3))
  // Pattern match to check which values were available
  match prev, next with
  | OptionalValue.Present(p), OptionalValue.Present(n) -> p + monthlyScale * (n - p) / 3.0
  | OptionalValue.Present(v), _
  | _, OptionalValue.Present(v) -> v
  | _ -> 0.0)


let volatility = 2.0 * interpVar ** 0.5

// create a frame to merge the Qly results with the monthly inputs
let ff = [ 
  "XSReturns" => XSReturn;
  "QlyVariance"  => variance;
  "InterpVariance" => interpVar;
  "Volatility" => volatility ] |> Frame.ofColumns

// TODO - now, what do we need to push back to the DB?