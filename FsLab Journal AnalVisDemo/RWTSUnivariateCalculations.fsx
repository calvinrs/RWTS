(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**
<a name="Top"></a>
RWTS - Univariate Statistics
================

Univariate - using only one variable.

For RWTS, that is to say focusing on one Asset at a time. So for now, we are excluding correlation-style calculations.

*)

(**
Required Packages
================
We will require references to:  

System - for DateTime operations  
 
Deedle - to arrange time series data into Key -> Value "Frames"  
*)
open System
open Deedle

(*** hide ***)
let root = __SOURCE_DIRECTORY__ + "/data/TSExampleData/"

(**
<a name="TSData"></a>
Time Series Data
==================
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
<a name="XSReturns"></a>
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
Note that in the RWTS database, "Economy" is denoted by an "E_XXX" asset. In our proper Time Series service, this will be an Economy code.  

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
<a name="UncVol"></a>
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

    
    // We can now base the variance series, and use Deedle's "Series.scanValues" to perform the rolling Moving average of squared returns. 
    // We add the initialisation point back on at the end.    
    let variance = 
        xsReturns   
        |> Series.filter (fun k v -> k.Equals(initDate) = false)  
        |> Series.scanValues (fun var ret -> lambda * var + (1.0 - lambda) * ret ** 2.0 ) initialVal
        |> Series.merge initPoint
    
    //Finally, we convert the variance to an annualised volatility. As we have quarterly returns, we multiply the quarterly vol (variance ^ 2) by the square root of the number of quarters per year (sqrt(12/3) = 2)    
    let volatility = 2.0 * variance ** 0.5
    volatility

// test this using our XS return series calculated earlier
let testVolatility = unconditionalVol setting_lambda setting_initialVal testXSReturn

(**
Again we can decide to push the data as a full time series, an update to an existing time series or just the latest value at the given calibration date.
*)

let finalOutputUncVol  = testVolatility.Get(DateTime(2016, 12, 31))
(*** include-value:finalOutputUncVol***)

(**
BONUS: We can create a function that takes a tuple of ("AssetName", "Economy", lambda, initialval) and produces the volatility series
*)

let getUnconditionalVol myTuple = 
    let (asset, econ, lambda, init) = myTuple
    let vol = unconditionalVol lambda init        
    getXSReturn (asset, econ) |> vol

let testgetUnconditionalVol = getUnconditionalVol ("E_AUD","E_AUD", 0.99, 0.007747769515651)
testgetUnconditionalVol.Get(DateTime(2016, 12, 31))


(**
<a name="OneMonthVol"></a>
EWMA One Month Vol and Implied One Month Vol tools
================
We have already implemented tools to calculate the one-month volatility targets.  

We use the "Implied One Month Vol" tool for assets where an implied volatility is published.  

Where no data is avaliable, we calculate a 1m vol using the excess return series. For this tool, we should refactor it so that it pulls in the Time Series data for excess returns
, like the Unconditional Volatility tool above.  

Note that these calibration tools are run on a "latest to date" basis using all data up to the CalibrationDate. There is no delta required here. 
*)

(**
<a name="TermStructure"></a>
SVJD Volatity term structure
================
We wish to run, on each calibration date, a tool to produce the volatility term structure for each asset.  

The SVJD term structure is a function of
+ some SVJD parameters
+ The latest 1-month volatility (at the Calibration Date)
+ The latest avaliable Unconditional Volatility

The unconditional vol will be from the last analysis, so will have a delta of -3M or -6M from the calibration date. Getting the "latest avaliable" value for the input should be sufficient here.  

The "DiscreteSVJDCumVol" function should be avaliable in some part of the SVJD tool, or better still within the common maths library.  

To complete this section, I have included a fresh definition of this function below.  
*)

// We can define the SVJD parameters
type SVJDParameters = {reversionLevel: float; reversionSpeed: float; startVal: float; volOfVariance: float; Correlation: float; jumpIntensity: float; jumpMean: float; jumpVol: float }
// and set these to their default values (these are updated yearly)
let defaultSVJDParameters = {reversionLevel = 0.25; reversionSpeed = 4.10636051632234; startVal = 0.2; volOfVariance = 0.468645736687236; Correlation = -0.527442089116611; jumpIntensity = 0.0; jumpMean = 0.0; jumpVol = 0.0}

// we will need to define the 1m and unconditional targets for the given asset
let this1mVol = 0.14551565323787
let thisUncVol = 0.282190830041867

// We use the square of these as the reversionLevel and volOfVariance SVJD parameters
let thisSVJDParamters = { defaultSVJDParameters with reversionLevel = thisUncVol * thisUncVol; startVal = this1mVol * this1mVol;}

// Set the constants we will need for extrapolations
let timescaleSVJD = 1.0 / 12.0
let returnMonths = [1;3;6;9;12;24;36;48;60;84;120;360]


// finally, we need to create a function to calculate the interpolated vols
let getSVJDInterpolatedVols parameters timescale monthlist = 
    
    let hestonVariance revLevel revSpeed currVol time = revLevel * (1.0 - Math.Exp(-revSpeed * time)) + currVol * Math.Exp(-revSpeed * time)

    let hestonVarianceOfVariance revLevel revSpeed currVol volVar time = 
        revLevel * volVar ** 2.0 * (1.0 - Math.Exp(-2.0 * revSpeed * time)) / (2.0 * revSpeed) + currVol * volVar ** 2.0 * (1.0 - revLevel / currVol) * Math.Exp(-revSpeed * time) * (1.0 - Math.Exp(-revSpeed * time)) / revSpeed
    
    let jumpVariance intensity mean vol = intensity * (mean * mean + vol * vol)

    let hestonCovariance revLevel revSpeed currVol volVar correl startTime endTime = 
        correl * ((hestonVariance revLevel revSpeed currVol endTime) * (hestonVarianceOfVariance revLevel revSpeed currVol volVar startTime)) ** 0.5

    let intervalVol parameters startTime endTime timescale = 
        hestonVariance parameters.reversionLevel parameters.reversionSpeed parameters.startVal startTime
        + 0.25 * timescale * hestonVarianceOfVariance parameters.reversionLevel parameters.reversionSpeed parameters.startVal parameters.volOfVariance startTime
        + jumpVariance parameters.jumpMean parameters.jumpMean parameters.jumpVol
        - hestonCovariance parameters.reversionLevel parameters.reversionSpeed parameters.startVal parameters.volOfVariance parameters.Correlation startTime endTime

    let sortedOutputMonths = monthlist |> List.sort
    let maxTermInMonths = monthlist |> List.max |> float

    let termsSVJD = [0.0..maxTermInMonths] |> List.map (fun t -> t * timescale)
    
    let intervalsSVJD = termsSVJD |> List.pairwise
    let intervalVols = intervalsSVJD |> List.map (fun (t1, t2) -> intervalVol parameters t1 t2 timescaleSVJD)
    
    let calcIntervalTotalVol subInt = (List.sum subInt / float subInt.Length) ** 0.5
    let takeSubInterval months = intervalVols |> List.take months
    let subIntTotalVol months = takeSubInterval months |> calcIntervalTotalVol

    monthlist |> List.map (fun m -> (m, subIntTotalVol m)) |> List.toSeq


// We can now call our function to return the interpolated term structure
let extrapolatedSVJDTermStructure = getSVJDInterpolatedVols thisSVJDParamters (1.0 / 12.0) returnMonths
(*** include-value: extrapolatedSVJDTermStructure ***)

(*** hide ***)
getSVJDInterpolatedVols { defaultSVJDParameters with reversionLevel = 0.163868733808375 * 0.163868733808375; startVal = 0.0822093 * 0.0822093;} (1.0 / 12.0) returnMonths

(**
<a name="EffectiveDate"></a>
Effective date
================
For most RWTS calculations, we update the latest values twice yearly in March and September. This analysis is done well in advance of the quarter end. 
In these cases, we use data only up to the end of the last quarter - so up to End-December and End-June for the above update cycles.  

To implement this, we want to define an "Effective Date" for the calibration relative to the "Calibration Date", and data will be truncated or sampled at the effective date.  

We will implement this as a "delta" setting within the tool of "-3M" that we will apply to the calibration date.  

Currently, we run RWTS each quarter. When we have broken this up into individual tools, we will only need to run these as required. So, the twice yearly updates will actually be run only every six months.  
*)