(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**
RWTS - Multivariate Statistics
================

Multivariate - using more than one variable.

For RWTS, that is to say focusing on multiple Asset at a time, focusing on the relationship between assets - i.e. correlation-style calculations.

*)

(**
Required Packages
================
We will require references to:  

System - for DateTime  

Deedle - to arrange time series data into Key -> Value "Frames"  

Mathnet.Numerics - for off the shelf statistical functions  

*)
open System
open Deedle

open MathNet.Numerics
open MathNet.Numerics.Statistics;

(*** hide ***)
let root = __SOURCE_DIRECTORY__ + "/data/TSExampleData/"

(**
Data Import
================
Like in the multivariate case , we can mock up a "database" of time series data by pulling in a full set of data, up to EndDec2016. 
Here, we will jump ahead to the "Annualised quarterly returns" series - this will mimic the process when we have a tool or service that will provide returns as a time series.  
The data here is the original RWTS total return results from the database.
*)

// I can create a function to return all <float> values for each Asset
let ReturnSeries (allSeries: Frame<DateTime,string>) asset = 
    allSeries.GetColumn<string>(asset) |> Series.filter (fun k v -> v <> "null") |> Series.map (fun k v -> float v)

let XSReturnsfromCSV = 
    Frame.ReadCsv<DateTime>(root + "HistoricReturnsQuarterly_EndDec2016_InColumns.csv", indexCol="Index", missingValues=[|"null"|])
    |> Frame.sortRowsByKey

let returnXSReturnSeries = ReturnSeries XSReturnsfromCSV

// We can now call this function to "import" our time series of data
let testXSRetSeries = returnXSReturnSeries "E_CNY"

(**
Historic 10Y Correlations
================
We can introduce how we will approach multivariate statistics by taking the simplest example.  
We wish to calculate "Historic 10Y Correlations", which are simply the correlation between asset returns over the last 10 years.  

So to calculate correlation between a number of assets, we will need to pull in all avaliable returns for our assets.
*)

let exampleReturnDataFrame = [ 
      "ASX_200_A_REIT" => returnXSReturnSeries "ASX_200_A_REIT";  
      "ASX_200_BANKS" => returnXSReturnSeries "ASX_200_BANKS" ;
      "China_25" => returnXSReturnSeries "China_25"] |> Frame.ofColumns

(**
To define the 10Y period, we will need to define our calibraiton date.
*)

let calibrationDate = DateTime(2016, 12, 31)
let tenYearsAgo = calibrationDate.AddYears(-10)

// slice on a series
testXSRetSeries.Between(tenYearsAgo, calibrationDate)
// or on the combined frame
let exampleHist10YReturns = exampleReturnDataFrame |> Frame.filterRows (fun k v -> k > tenYearsAgo && k <= calibrationDate)

(**
Now that we have identically alligned time series, and assuming no missing values, we can take these series and calculate the correlation between them.
*)

let myX = exampleHist10YReturns.GetColumn<float>("ASX_200_A_REIT").ValuesAll
let myY = exampleHist10YReturns.GetColumn<float>("ASX_200_BANKS").ValuesAll
let myZ = exampleHist10YReturns.GetColumn<float>("China_25").ValuesAll

let myCorrelXY = Statistics.Correlation.Pearson(myX, myY)
let myCorrelXZ = Statistics.Correlation.Pearson(myX, myZ)
let myCorrelYZ = Statistics.Correlation.Pearson(myY, myZ)

(**
We can arrange this data into a matrix if we wish, using the LinearAlgebra package's matrix type.
*)

open MathNet.Numerics.LinearAlgebra;

let correlMatrix = matrix [ [1.0; myCorrelXY; myCorrelXZ]
                            [myCorrelXY; 1.0; myCorrelYZ]
                            [myCorrelXZ; myCorrelYZ; 1.0] ]

(**
BONUS: We can test if a matrix is PSD like so:
*)
let isPSD (matrix: Matrix<float>) = matrix.Evd().EigenValues.Real() |> Seq.min >= 0.000001
isPSD correlMatrix

(**
We do not need to test this for our historic 10Y correlation matrix - its construction shoud ensure that this is true.
*)

(**
Unconditional Correlations
================
A slightly tricker calculation is the way in which we set our unconditional correlations.  

Like the historic correlations, we need to work with pairs of time series. 
For this calculation however, we need to be careful about the time period we are working over, conditional on the lifespan of both series in the pair.  

For this example, we can go back in time to the pre-DB RWTS sheet, so we can compare results. The TR series are slightly different, but we should get a similar result.  
*)

// for the EWMA part of this, we will need to determine the initialisation value and the lambda value.

// lambda can be expressed as a value, or in terms of the Mean age of the data (25y) and the observations per year (4 for quarterly data) 
let uncCorrelLambda = (1.0 - (1.0/4.0) / 25.0) //0.99

// the initialisation value is 0.005625, but will be scaled based on the asset type (equity or bond) and if the underlying economy matches for the assets
// in our example case, we will investigate equity assets that have different underlying economies, which gives us a scaling factor of 0.5
let uncVarInit = 0.005625
let uncCovarInit = uncVarInit * 0.5

// Set the calibration date
let oldCalibrationDate = DateTime(2009, 12, 31)

// We will take a pair of TR series, and truncate these to include data up to the calibration date only
let seriesLH =  (returnXSReturnSeries "E_AUD").EndAt(oldCalibrationDate)
let seriesRH = (returnXSReturnSeries "E_HKD").EndAt(oldCalibrationDate)

// we will need to take the mean value for each series. This is over the entire lifespan of each series. 
// the Deedle Series class has its own univariate statistics, which we can use for simple stuff like mean, standard dev. etc.
let seriesMeanLHS = seriesLH.Mean()
let seriesMeanRHS = seriesRH.Mean()

// For the EWMA part of this calculation, we need to start at the first point in time where both series are avaliable
let combinedStartDate = if seriesLH.FirstKey() > seriesRH.FirstKey() then seriesLH.FirstKey() else seriesRH.FirstKey()

// we can now truncate the data series, and from now on will be working with series of equal length
let ewmaReturnsLH = seriesLH.StartAt(combinedStartDate)
let ewmaReturnsRH = seriesRH.StartAt(combinedStartDate)

// Use the Series mu (over all points) rather than the mean of the ewma period, to calculate the covariance
let seriesLHMinusMu = ewmaReturnsLH - seriesMeanLHS
let seriesRHMinusMu = ewmaReturnsRH - seriesMeanRHS

let ewmaCovar = seriesLHMinusMu * seriesRHMinusMu

(**
We can use Deedle's "Series.scanValues" to perform the EWMA weighted Covariance and series Variances.
*)
let ewmaWeightedCovar = 
    ewmaCovar      
    |> Series.scanValues (fun lastweightedCovar thisCovar -> uncCorrelLambda * lastweightedCovar + (1.0 - uncCorrelLambda) * thisCovar ) uncCovarInit
    
let ewmaVarLH = 
    seriesLHMinusMu      
    |> Series.scanValues (fun lastweightedVar thisVar -> uncCorrelLambda * lastweightedVar + (1.0 - uncCorrelLambda) * thisVar ** 2.0 ) uncVarInit

let ewmaVarRH = 
    seriesRHMinusMu      
    |> Series.scanValues (fun lastweightedVar thisVar -> uncCorrelLambda * lastweightedVar + (1.0 - uncCorrelLambda) * thisVar ** 2.0 ) uncVarInit

(**
We can now calculate the correlation based on weighted covariance and variance.
*)

let ewmaCorrel = ewmaWeightedCovar / (ewmaVarLH * ewmaVarRH) ** 0.5

// finally, the contribution to the current unconditional correlation matrix for this pair is the ewmaCorrel value at the calibration date

let finalUnconditionalCorrelation = ewmaCorrel.Get(oldCalibrationDate)

(**
Refactoring Into Pair functions
================

For mostly my own benifit, I want to see how we can use this code to calculate an entire correlation matrix.
*)

// Refactor into a function that returns correlation from a pair (correlation with itself is 1.0)
let get10YCorrelFromPair (calibrationDate: DateTime) assetLH assetRH = 
    
    match assetLH = assetRH with
    | true -> 1.0
    | _ -> 
            // To ignore missing data, we need to truncate the trailing missing values
            let seriesLH = returnXSReturnSeries assetLH
            let seriesRH = returnXSReturnSeries assetRH
            let combinedStartDate = if seriesLH.FirstKey() > seriesRH.FirstKey() then seriesLH.FirstKey() else seriesRH.FirstKey()

            let exampleReturnDataFrame = Frame.ofColumns [ assetLH => seriesLH; assetRH => seriesRH ]  
                
            let tenYearsAgo = calibrationDate.AddYears(-10)  
              
            let exampleHist10YReturns = exampleReturnDataFrame |> Frame.filterRows (fun k v -> k >= combinedStartDate && k > tenYearsAgo && k <= calibrationDate)
                         
            let myX = exampleHist10YReturns.GetColumn<float>(assetLH).ValuesAll
            let myY = exampleHist10YReturns.GetColumn<float>(assetRH).ValuesAll

            let myCorrelXY = Statistics.Correlation.Pearson(myX, myY)

            myCorrelXY

// This should return the same values as above
get10YCorrelFromPair calibrationDate "ASX_200_A_REIT" "ASX_200_BANKS" // = 0.6329625501
get10YCorrelFromPair calibrationDate "ASX_200_A_REIT" "China_25"  // = 0.4239930683
get10YCorrelFromPair calibrationDate "China_25" "China_25" // = 1.0
// pair with a missing value 
get10YCorrelFromPair calibrationDate "EURSTOXX50" "EUR_BankLoans" // 0.5962678681

// We can now define a List of all assets we wish to include in the correlation matrix
let assetList = ["ASX_200_A_REIT";"ASX_200_BANKS";"China_25"] 

// We will create a record type to hold all the info about a "matrix"
type MatrixDescription = {assets: List<string>; matrix: Matrix<float>}

// We want to yield all pairs of assets, and calculate the correlation between them
let getCorrelMatrix calibrationDate funCorrelMethod assetList = 

    let getThis10YCorrelFromPair = funCorrelMethod calibrationDate 
    let indexedAssetList = assetList |> List.zip [0..assetList.Length-1]

    let flatMatrix = seq { for keyLH, assetLH in indexedAssetList do
                            for keyRH, assetRH in indexedAssetList do
                                yield (keyLH, keyRH, getThis10YCorrelFromPair assetLH assetRH)
                         }
    let denseMatrix = DenseMatrix.ofSeqi assetList.Length assetList.Length flatMatrix
    let result = {assets = assetList; matrix = denseMatrix}
    result
                    //yield (assetLH, assetRH, getThis10YCorrelFromPair assetLH assetRH)
      

let test10YMatrix = getCorrelMatrix calibrationDate get10YCorrelFromPair assetList

(**
We can repeat the same approach for the unconditional correlation
*)

let getUncCorrelFromPair lambda uncVarInit covarScale (calibrationDate: DateTime) assetLH assetRH =
    
    match assetLH = assetRH with
    | true -> 1.0
    | _ -> 
        let seriesLH =  (returnXSReturnSeries assetLH).EndAt(calibrationDate)
        let seriesRH = (returnXSReturnSeries assetRH).EndAt(calibrationDate)

        // we will need to take the mean value for each series. This is over the entire lifespan of each series.   
        let seriesMeanLHS = seriesLH.Mean()
        let seriesMeanRHS = seriesRH.Mean()

        // For the EWMA part of this calculation, we need to start at the first point in time where both series are avaliable
        let combinedStartDate = if seriesLH.FirstKey() > seriesRH.FirstKey() then seriesLH.FirstKey() else seriesRH.FirstKey()

        // we can now truncate the data series, and from now on will be working with series of equal length
        let ewmaReturnsLH = seriesLH.StartAt(combinedStartDate)
        let ewmaReturnsRH = seriesRH.StartAt(combinedStartDate)

        // Use the Series mu (over all points) rather than the mean of the ewma period, to calculate the covariance
        let seriesLHMinusMu = ewmaReturnsLH - seriesMeanLHS
        let seriesRHMinusMu = ewmaReturnsRH - seriesMeanRHS
        let ewmaCovar = seriesLHMinusMu * seriesRHMinusMu

        // EWMA of series
        let ewmaWeightedCovar = 
            ewmaCovar      
            |> Series.scanValues (fun lastweightedCovar thisCovar -> lambda * lastweightedCovar + (1.0 - lambda) * thisCovar ) uncCovarInit
    
        let ewmaVarLH = 
            seriesLHMinusMu      
            |> Series.scanValues (fun lastweightedVar thisVar -> lambda * lastweightedVar + (1.0 - lambda) * thisVar ** 2.0 ) uncVarInit

        let ewmaVarRH = 
            seriesRHMinusMu      
            |> Series.scanValues (fun lastweightedVar thisVar -> lambda * lastweightedVar + (1.0 - lambda) * thisVar ** 2.0 ) uncVarInit

        let ewmaCorrel = ewmaWeightedCovar / (ewmaVarLH * ewmaVarRH) ** 0.5
        // finally, the contribution to the current unconditional correlation matrix for this pair is the ewmaCorrel value at the calibration date
        ewmaCorrel.Get(calibrationDate)

// We will (incorrectly) assume that the constants are the same for each pair of assets - in practice we will need more asset info to get the scaling factor out
let defaultGetUncCorrelFromPair = getUncCorrelFromPair 0.99 0.005625 0.5

// And we can now return the Unc. correlation as above
defaultGetUncCorrelFromPair oldCalibrationDate "E_AUD" "E_HKD"

// Finally, we can use this function to get a "matrix" of Unconditional correlations
let testUncMatrix = getCorrelMatrix calibrationDate defaultGetUncCorrelFromPair assetList

// A few more test cases
get10YCorrelFromPair oldCalibrationDate "E_GBP" "E_USD" 
defaultGetUncCorrelFromPair oldCalibrationDate "E_GBP" "E_USD"


(**
Let's try some more speculative stuff.  

We can import the full portfolio list, and calculate the full correlation matrix.  

We are ignoring the individual scaling parameters for now.  
*)

let fullAssetInfo = 
    Frame.ReadCsv<string>(root + "AssetsList_Prod_EndDec2016.csv", indexCol="Economy")
    |> Frame.sortRowsByKey

let full10YMatrix = getCorrelMatrix calibrationDate get10YCorrelFromPair (fullAssetInfo.RowKeys |> Seq.toList)

let fullUncMatrix = getCorrelMatrix calibrationDate defaultGetUncCorrelFromPair (fullAssetInfo.RowKeys |> Seq.toList)


(**
For the final output matrix (after E_EUR adjustment), we need to ensure that the matrix is PSD.  

Can we call into our own "PSDCorrelationMatrix" function to make this so?
*)

isPSD fullUncMatrix.matrix
fullUncMatrix.matrix.Evd().EigenValues.Real() |> Seq.toList

//(full10YMatrix.matrix |> Matrix.toFrame).SaveCsv(root + "Output_10Y_Matrix.csv")
//(fullUncMatrix.matrix |> Matrix.toFrame).SaveCsv(root + "Output_Unconditional_Matrix.csv")


(**
Adding E_EUR and E_SKK
================

TO BE DETERMINED IF WE NEED TO DO THIS.

*)

(**
Portfolio Calculations
================

Given the (final) correlation matrix, and the individual asset Unconditional Volatility numbers, we can move on to calculate the Beta, Market Price of Risk and Risk Premium.

In this section, I will show a very small set of assets as an example case. The market numbers here are made up for example purposes.

We must first define our global equity asset portfolio. This comprises of:

*)

// we have a matrix and a list of assets from above
testUncMatrix.matrix
testUncMatrix.assets

// We will need to pull in the Unconditional vols for each asset. Here, we will make up some values for the example
let unconditionalVols = [0.1875;0.165;0.2810]

// The assumed return on the market portfolio
let marketPfReturn = 0.04

// The market capitalisation of each asset, at the current calibration date (this will be the latest value in the underlying time series)
let marketCap = [1000.0;200.0;1500.0] |> List.zip assetList |> Map.ofList

// The portfolio weight is the ratio of the asset's market cap to the global total, multiplied by the 1 or 0 setting value that determines if the asset is in the PF
// We will assume that all of these assets are in the portfolio
let totalMarketCap = marketCap |> Map.toList |> List.map (fun (k1,v1) -> v1) |> List.sum
let portfolioWeight = marketCap |> Map.map (fun k v -> v / totalMarketCap )

// To calculate the covariance, we need to use some matrix multiplication
// So, we will convert the weights into a n * 1 matrix (or if you like, a vector)
let pfWeightsMatrix = matrix [ assetList |> List.map (fun asset -> portfolioWeight.[asset]) ] |> Matrix.transpose

let pfCovariance = (testUncMatrix.matrix * pfWeightsMatrix).Column(0)

// the market portfolio variance is equal to wT * Covar * w
let marketPfVariance = (pfWeightsMatrix.Transpose() * testUncMatrix.matrix * pfWeightsMatrix).[0,0]

// The market Beta for each asset is equal to covar/porfolioVar
let marketBetas = pfCovariance |> Seq.map (fun cov -> cov / marketPfVariance)

// The risk premia can be calculated, these are equal to beta * pfReturn
let riskPremia = marketBetas |> Seq.map (fun beta -> beta * marketPfReturn)

// The market price of risk (or Z-score) is equal to the risk premia / unconditional vol
riskPremia |> Seq.toList |> List.zip unconditionalVols |> List.map (fun (uncVol, riskPrem) -> riskPrem / uncVol)

(**
Real-life example
-----------------

We can import market caps for avaliable assets, and perform this analysis on the actual EndDec2016 data.
*)

let marketCapInfo = 
    Frame.ReadCsv<string>(root + "MarketCaps_EndDec2016.csv", indexCol="Economy")
    |> Frame.sortRowsByKey

// we can now extend the fullAssetInfo to include this - missing market caps should be set to zero
let extendAssetInfoWithCaps info caps = 
    let withCaps = Frame.join JoinKind.Left info caps |> Frame.fillMissingWith 0.0 
    let marketCapTotal = withCaps?MarketCap * withCaps?In_Market_Portfolio |> Stats.sum
    let weightInPf = withCaps?MarketCap / marketCapTotal
    withCaps?weightInPf <- weightInPf
    withCaps

let fullAssetInfoWithCaps = extendAssetInfoWithCaps fullAssetInfo marketCapInfo

(**
Adding the E_EUR asset
================
*)

// We can query the asset list to get all assets that are in the E_EUR synthetic asset
fullAssetInfo |> Frame.filterRowValues(fun row -> row.GetAs<float>("In_Euro") = 1.0)

