(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**
RWTS - Univariate Statistics
================

Univariate - using only one variable.

For RWTS, that is to say focusing on one Asset at a time. So for now, we are excluding correlation-style calculations.

*)

(**
Required Packages
================
We will require references to:
System - for DateTime
Deedle - to arrange time series data into Key -> Value "Frames"
*)
open System
open Deedle

(*** hide ***)
let root = __SOURCE_DIRECTORY__ + "/data/TSExampleData/"

(**
We can mock up a "database" of time series data by pulling in a full set of data, up to EndDec2016
*)

//All series
let TRIfromCSV = 
    Frame.ReadCsv<DateTime>(root + "TotalReturnIndices_EndDec2016_InColumns.csv", indexCol="Index", missingValues=[|"null"|])
    |> Frame.sortRowsByKey

let ThreeMRatesfromCSV = 
    Frame.ReadCsv<DateTime>(root + "ThreeMonthInterestRates_EndDec2016_InColumns.csv", indexCol="Index", missingValues=[|"null"|])
    |> Frame.sortRowsByKey

// I can create a function to return all <float> values for each Asset
let ReturnSeries (allSeries: Frame<DateTime,string>) asset = 
    allSeries.GetColumn<string>(asset) |> Series.filter (fun k v -> v <> "null") |> Series.map (fun k v -> float v)

// And I can "bake in" the series to have a 1-argument function to return the required series

let returnTRISeries = ReturnSeries TRIfromCSV
let return3MSeries = ReturnSeries ThreeMRatesfromCSV


let ASX_200_A_REIT_TRI = returnTRISeries "ASX_200_A_REIT"
let AUDRates = return3MSeries "E_AUD"


(**
Initial calculations
================
For most RWTS calculations, we will need a time series of "Annualised Quarterly Excess Returns".
In current RWTS, this is calculated in each RWTS run and pushed to the "[RWTargetSetting].[Calculated].[tblHistoricReturnsQuarterly]" table.
The underlying data series are "[RWTargetSetting].[MarketData].[tblTotalReturnIndex]" and "[RWTargetSetting].[MarketData].[tblThreeMonthInterestRates]". These series store the latest values only.

We will start by loading our required data
*)

(**
Annualised Quarterly Excess Returns
================
We will start by loading our required data for TRI and 3M rates.
The TRI will be for the Asset we want to calibrate. The 3M rates will be for the economy underlying the Equity Asset - these are labelled "E_XXX" in our DB, but should just be "XXX".

For the tool proper, we will pull in data from the relevant "TimeSeries".
Here, we will pull in data from our mock DB, like so.
*)

let TRI = returnTRISeries "E_CNY"
let Rates3M = return3MSeries "E_CNY"

(**
So, to calculate the Annualised Quarterly Excess Returns, we can do the following: 
*)

let XSReturn (tri: Series<DateTime,float>) (threeMRates: Series<DateTime,float>) = 
    // Calculate log changes over 3M in TRI, and drop the first 3m where we cannot compare
    let logTRQtrly = log(tri) - log(tri.Shift(3)) |> Series.dropMissing

    // get the 3M rates, that we want to be in XS of
    // rates have already been converted
    // convert to a continuously compounded 3M rate
    // TODO - Fill missing with CPI changes, for the economies that have missing short rates
    let qtrlyRate = 0.25 * log(1.0 + threeMRates.Shift(3))

    // We need to truncate the series where we do not have a total return - we are ok when we have a total return, but no 3m rate
    let initDate = logTRQtrly.GetKeyAt(0)
    let truncQtrlyRate = qtrlyRate |> Series.filter (fun k v ->  k >= initDate)

    let joinTRITo3m = [ 
      "LogTRQtrly" => logTRQtrly;  
      "QtrlyRate" => truncQtrlyRate ] |> Frame.ofColumns

    // Calculate the XS return over the 3m rate - when there are no rates, use the plain return. There should only be missing values on the Right hand side
    let fullXSReturns = joinTRITo3m?LogTRQtrly - joinTRITo3m?QtrlyRate |> Series.fillMissingUsing (fun k -> joinTRITo3m?LogTRQtrly.Get(k))

    //finally, we only want to return values for each quarter end
    fullXSReturns |> Series.filter (fun k v -> k.Month % 3 = 0)


let testXSReturn = XSReturn TRI Rates3M

(**
To check, we can test this against our DB values of XS returns. This can also be imported from CSV.
*)

let XSReturnsfromCSV = 
    Frame.ReadCsv<DateTime>(root + "HistoricReturnsQuarterly_EndDec2016_InColumns.csv", indexCol="Index", missingValues=[|"null"|])
    |> Frame.sortRowsByKey

let returnXSReturnSeries = ReturnSeries XSReturnsfromCSV

let joinOurCalcsToDBCalcs = [ 
      "Ours" => testXSReturn;  
      "Db" => returnXSReturnSeries "E_CNY" ] |> Frame.ofColumns

(*** include-value: joinOurCalcsToDBCalcs ***)

(**
We can now decide what we want to do with this result. We can push the data as a full time series, or as an update to an existing time series.

We can now use the time series of excess returns to calculate the RWTS stats that we need.
*)

(**
BONUS: We can create a function that takes a tuple of ("AssetName", "Economy") and produces the XS return series
*)

let getXSReturn myTuple = 
    let (asset, econ) = myTuple    
    XSReturn (returnTRISeries asset) (return3MSeries econ)

getXSReturn ("E_AUD","E_AUD")

(**
Unconditional Volatility
================

Unconditional Volatility is the EWMA volatilty of the Annualised Quarterly Excess Returns series.

As we need to calculate a rolling EWMA over the series, we need to define our settings for lambda, and the initialisation value for the variance of the series.  
*)

let setting_lambda = 0.98
let setting_initialVal= 0.031102706532478

(**
To perform a EWMA over the series, we need to initialise the new "variance" series at the starting point.
*)

let unconditionalVol lambda initialVal (xsReturns: Series<DateTime,float>)= 

    let initDate = xsReturns.GetKeyAt(0)
    let initPoint = Series([initDate], [initialVal])

    (**
    We can now base the variance series, and use Deedle's "Series.scanValues" to perform the rolling Moving average of squared returns. We add the initialisation point back on at the end.
    *)
    let variance = 
        xsReturns   
        |> Series.filter (fun k v -> k.Equals(initDate) = false)  
        |> Series.scanValues (fun var ret -> lambda * var + (1.0 - lambda) * ret ** 2.0 ) initialVal
        |> Series.merge initPoint
    (**
    Finally, we convert the variance to an annualised volatility. As we have quarterly returns, we multiply the quarterly vol (variance ^ 2) by the square root of the number of quarters per year (sqrt(12/3) = 2)
    *)
    let volatility = 2.0 * variance ** 0.5
    volatility

// test this using our XS return series calculated earlier
let testVolatility = unconditionalVol setting_lambda setting_initialVal testXSReturn

(*** include-value: testVolatility ***)

(**
Again we can decide to push the data as a full time series, an update to an existing time series or just the latest value at the given calibration date.
*)

testVolatility.Get(DateTime(2016, 12, 31))

(**
BONUS: We can create a function that takes a tuple of ("AssetName", "Economy", lambda, initialval) and produces the volatility series
*)

let getUnconditionalVol myTuple = 
    let (asset, econ, lambda, init) = myTuple
    let vol = unconditionalVol lambda init        
    getXSReturn (asset, econ) |> vol

let testgetUnconditionalVol = getUnconditionalVol ("E_AUD","E_AUD", 0.99, 0.007747769515651)
testgetUnconditionalVol.Get(DateTime(2016, 12, 31))