(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**
<a name="Top"></a>
RWTS - Multivariate Statistics
================

Multivariate - using more than one variable.

For RWTS, that is to say focusing on multiple Assets together, focusing on the relationship between assets - i.e. correlation-style calculations.  

We will define a _portfolio_ of Assets that we will use to determine the coverage. From this definition, we can return some additional portfolio statistics.  

*)

(**
Required Packages
================
We will require references to:  

System - for DateTime. 

Deedle - to arrange time series data into Key -> Value "Frames", and for univariate statistical functions.  

Mathnet.Numerics - for off the shelf multivariate statistical functions, and for Matrix algebra.

*)
open System
open Deedle

open MathNet.Numerics
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra

(*** hide ***)
let root = __SOURCE_DIRECTORY__ + "/data/TSExampleData/"

(**
<a name="import"></a>
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
<a name="tenyearcorrel"></a>
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

let correlMatrix = matrix [ [1.0; myCorrelXY; myCorrelXZ]
                            [myCorrelXY; 1.0; myCorrelYZ]
                            [myCorrelXZ; myCorrelYZ; 1.0] ]
(*** include-value:correlMatrix***)

(**
BONUS: We can test if a matrix is PSD like so:
*)
let isPSD (matrix: Matrix<float>) = matrix.Evd().EigenValues.Real() |> Seq.min >= 0.000001
isPSD correlMatrix

(**
We do not need to test this for our historic 10Y correlation matrix.
*)

(**
<a name="unccorrel"></a>
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
We can use Deedle's "Series.scanValues" function to perform the EWMA weighted Covariance and series Variances.
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
-------------------------------

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
(*** include-value:test10YMatrix***)

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

        let uncCovarInit = uncVarInit * covarScale

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

// We will (possibly incorrectly, but this matches the RWTS DB code) assume that the constants are the same for each pair of assets 
// - in practice we may need the underlying economy info to get the scaling factor out
let defaultGetUncCorrelFromPair = getUncCorrelFromPair 0.99 0.005625 0.5

// And we can now return the Unc. correlation as above
defaultGetUncCorrelFromPair oldCalibrationDate "E_AUD" "E_HKD" //  = 0.53407839

// Finally, we can use this function to get a "matrix" of Unconditional correlations
let testUncMatrix = getCorrelMatrix calibrationDate defaultGetUncCorrelFromPair assetList
(*** include-value:testUncMatrix***)

// A few more test cases
get10YCorrelFromPair oldCalibrationDate "E_GBP" "E_USD" 
defaultGetUncCorrelFromPair oldCalibrationDate "E_AUD" "E_GBP"
defaultGetUncCorrelFromPair oldCalibrationDate "E_GBP" "E_USD"


(**
<a name="fullmatrix"></a>
Full correlation matrix from portfolio information
================
Let's try some more speculative stuff.  

We can import the full portfolio list, and calculate the full correlation matrix.  

We are ignoring the individual scaling parameters for now.  
*)

let fullAssetInfo = 
    Frame.ReadCsv<string>(root + "AssetsList_Prod_EndDec2016.csv", indexCol="Economy")
    |> Frame.sortRowsByKey
(*** include-value:fullAssetInfo***)

(**
We can calculate the correlation matrices for all required assets in the input portfolio.
*)
let full10YMatrix = getCorrelMatrix calibrationDate get10YCorrelFromPair (fullAssetInfo.RowKeys |> Seq.toList)
(*** include-value:full10YMatrix.matrix***)

let fullUncMatrix = getCorrelMatrix calibrationDate defaultGetUncCorrelFromPair (fullAssetInfo.RowKeys |> Seq.toList)
(*** include-value:fullUncMatrix.matrix***)


(**
<a name="eurextend"></a>
Adding the E_EUR asset
================

In order to calculate the E_EUR asset, we will need to be able to switch between a correlation matrix and volatility vector to a covariance matrix and back again.  

We can use the Matrix type in the MathNet.Numerics.LinearAlgebra package to produce the helper functions that we need.  
*)

// We can define some simple example correlation, covariance and volatility(sigma) parameters
let correl = matrix [[1.0;-0.5]
                     [-0.5;1.0]]

let sigma = vector [0.5;2.0]

let cov = matrix  [[0.25;-0.5]
                   [-0.5;4.0]]

// We can switch to covariance given the correlation matrix and volatility vector
// Where covar(i,j) = corr(i,j) * var(i) * var(j)
let cor2cov (cor: Matrix<float>) (vol: Vector<float>) = 
    let varmatrix = vol.ToColumnMatrix() * vol.ToRowMatrix()
    varmatrix .* cor

let calcCov = cor2cov correl sigma
calcCov = cov

// And we can switch back again from the covariance matrix to produce the correlation and vol vector as a tuple (matrix * vector)
// Where vol(i) = var(i) ** 2 (take the diagonal of the covar matrix  to get variances)
// and corr(i,j) = covar(i,j) / vol(i)
let cov2corVol (cov: Matrix<float>) = 
    let vol = cov.Diagonal() |> Vector.map (fun x -> x ** 0.5)
    let varmatrix = vol.ToColumnMatrix() * vol.ToRowMatrix()
    let calcCorr = cov ./ varmatrix
    (calcCorr, vol)

let (calcCor, calcVol) = cov2corVol calcCov

calcVol = sigma
calcCor = correl

// To show this in action, we can use the small test matrix from above, and the corresponding vol vector
let testCorrMatrix = testUncMatrix.matrix
(*** include-value:testCorrMatrix***)

// And we can take some example Unconditional Volatility values, and create a vector of these
let testVolVector = [0.1875;0.165;0.2810] |> List.toSeq |> Vector.Build.DenseOfEnumerable
(*** include-value:testVolVector***)

// We can now convert these to a covariance matrix
let innerTestCovar = cor2cov testUncMatrix.matrix testVolVector
(*** include-value:innerTestCovar***)

(**
To add the E_EUR asset to this matrix, we need to know the market cap adjusted weight of each asset in the synthetic E_EUR asset.  

We can declare this for our test list of assets as:
*)

// An example weighting scheme for the definition of the E_EUR asset
let testEURWeights = Vector.Build.DenseOfEnumerable([0.5;0.25;0.25] |> List.toSeq )

// The E_EUR Vs. Other asset covariance can then be calculated 
// EUR_Covar(i) = weightInEUR * covar(i)
let testEURVsAssetCovar = testEURWeights.ToRowMatrix() * innerTestCovar

// Given this, we can use the same function to calculate the E_EUR Vs E_EUR covariance (or variance, as it is with itself)
let testEURVariance = testEURWeights.ToRowMatrix() * testEURVsAssetCovar.Transpose()

(**
We can now assemble the full covar matrix with E_EUR by joining the pieces together
*)

let testInnerCovarWithEURTopRow = testEURVsAssetCovar.Stack(innerTestCovar)
let testFullTransposedEURVsAssetCovar = testEURVariance.Stack(testEURVsAssetCovar.Transpose())
let testFullCovarWithEUR = testFullTransposedEURVsAssetCovar.Append(testInnerCovarWithEURTopRow)
(*** include-value:testFullCovarWithEUR***)

(**
We can now calculate:
+ The E_EUR asset unconditional volatility, and append this to the volatility vector
+ The full correlation matrix, including E_EUR
*)

// And we can now use the full matrix and full vol vector to calculate the correlation matrix
// This will also give us the volatilty vector, so we have a full vector that matches the full covar matrix
let (testFullCorr, testFullVolVectorWithEUR) = cov2corVol testFullCovarWithEUR
(*** include-value:testFullCorr***)
(*** include-value:testFullVolVectorWithEUR***)

// finally, we can pick up the E_EUR volatilty as the first entry in the new volatility vector
let testEURUncVol = testFullVolVectorWithEUR.[0]
(*** include-value:testEURUncVol***)

(**
The E_EUR vol will need to be pushed to the database as the "Unconditional Vol" for this asset.
*)


(*** hide ***)
fullAssetInfo |> Frame.filterRowValues(fun row -> row.GetAs<float>("In_Euro") = 1.0)

(**
Adding E_SKK
================

TO BE DETERMINED IF WE NEED TO DO THIS.

*)

(**
<a name="portfolio"></a>
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
let zScores = riskPremia |> Seq.toList |> List.zip unconditionalVols |> List.map (fun (uncVol, riskPrem) -> riskPrem / uncVol)

(*** hide ***)
let allPortfolioFunction pfReturn (marketCapWeights: Vector<float>) (unconditionalVols: Vector<float>) (correlationMatrix: Matrix<float>) info = 
    let pfCovariance = (correlationMatrix * marketCapWeights.ToColumnMatrix()).Column(0)
    let marketPfVariance = (marketCapWeights.ToRowMatrix() * correlationMatrix * marketCapWeights.ToColumnMatrix()).[0,0]
    let marketBetas = pfCovariance |> Seq.map (fun cov -> cov / marketPfVariance) |> Vector.Build.DenseOfEnumerable
    let riskPremia = marketBetas |> Seq.map (fun beta -> beta * pfReturn) |> Vector.Build.DenseOfEnumerable
    let zScores = riskPremia ./ unconditionalVols
    info // append results to the frame

(**
<a name="marketcaps"></a>
Extending the Portfolio Definition to include market caps
-----------------

We can import market caps for avaliable assets, and perform this analysis on the actual EndDec2016 data.
*)

let marketCapInfo = 
    Frame.ReadCsv<string>(root + "MarketCaps_EndDec2016.csv", indexCol="Economy")
    |> Frame.sortRowsByKey


(**
We can now extend the fullAssetInfo to include this - missing market caps should be set to zero.  
We can set the weight in the portfolio for each asset.
Taking this further, we can get the weight in euro in much the same way .
*)

(**
Taking this further, we can get the weight in euro in much the same way 
*)

let extendAssetInfoWithCaps info caps = 
    let withCaps = Frame.join JoinKind.Left info caps |> Frame.fillMissingWith 0.0 
    let marketCapTotal = withCaps?MarketCap * withCaps?In_Market_Portfolio |> Stats.sum
    let euroCapTotal = withCaps?MarketCap * withCaps?In_Euro |> Stats.sum
    let weightInPf = withCaps?MarketCap * withCaps?In_Market_Portfolio / marketCapTotal
    let weightInEuroPf = withCaps?MarketCap * withCaps?In_Euro  / euroCapTotal
    withCaps?weightInPf <- weightInPf
    withCaps?weightInEuroPf <- weightInEuroPf
    withCaps

let fullAssetInfoEuroCaps = extendAssetInfoWithCaps fullAssetInfo marketCapInfo

// We can confirm that the weights sum to one in both cases
fullAssetInfoEuroCaps?weightInPf |> Stats.sum
fullAssetInfoEuroCaps?weightInEuroPf |> Stats.sum

// And we can access the weights as a vector for use in the portfolio calculations
let pfWeightsVector = fullAssetInfoEuroCaps?weightInPf |> Series.values |> Vector.Build.DenseOfEnumerable


(**
<a name="psd"></a>
Full Matrix PSD adjustment.
================

We can use eigen decomposition to pull apart and rebuild a correlation matrix like so:  

*)



let decomposeMatrix (correlMatrix: Matrix<float>) = 
    let eigenDecomp = correlMatrix.Evd()
    let eigenValues = eigenDecomp.EigenValues.Real()
    let eigenVectors = eigenDecomp.EigenVectors
    let isPSD = eigenValues |> Seq.min >= 0.000001
    (eigenValues, eigenVectors, isPSD)

let (testEigenValues,testEigenVectors, testIsPSD) = decomposeMatrix testFullCorr

let reconstructMatrixFromEigenDecomposition (eigenVectors: Matrix<float>) eigenValues = eigenVectors * DenseMatrix.ofDiag(eigenValues) * eigenVectors.Inverse()

let reconstructedTestMatrix = reconstructMatrixFromEigenDecomposition testEigenVectors testEigenValues

(**
For the final output matrix (after E_EUR adjustment), we need to ensure that the matrix is PSD.  

Can we call into our own "PSDCorrelationMatrix" function to make this so?
*)

isPSD fullUncMatrix.matrix
fullUncMatrix.matrix.Evd().EigenValues.Real() |> Seq.toList
// if the matrix is not PSD, we can make it so by

// set the newEigenValue array

// Find the smallest positive eigenValue
let nThSmallestPosEigenValue eigenValues n = eigenValues |> Vector.toList |> Seq.filter (fun ev -> ev > 0.000001) |> Seq.sort |> Seq.item (n - 1)
let replaceNegativeEigenValues eigenValues replacement = eigenValues |> Vector.map (fun ev -> if ev <= 0.000001 then replacement else ev )

let latestSmallestPositiveEigenValue = nThSmallestPosEigenValue testEigenValues 1

let newPossibleEigenValues = replaceNegativeEigenValues testEigenValues latestSmallestPositiveEigenValue

let newPossibleCorrelMatrix = reconstructMatrixFromEigenDecomposition testEigenVectors newPossibleEigenValues

// We need to set the diagonal back to one
let newDiagonalOfOnes = testEigenValues |> Vector.map (fun e -> 1.0)

newPossibleCorrelMatrix.SetDiagonal(newDiagonalOfOnes)

// We can now check if the adjusted matrix is PSD
let (newEigenValues,newEigenVectors, newIsPSD) = decomposeMatrix newPossibleCorrelMatrix

// if not PSD, then we need to recursively set to the next smallest EigenValue...

// a function to do this can be defined like so...
let ensureMatrixIsPSD matrix = 

    let nThSmallestPosEigenValue eigenValues n = eigenValues |> Vector.toList |> Seq.filter (fun ev -> ev > 0.000001) |> Seq.sort |> Seq.item (n - 1)
    let replaceNegativeEigenValues eigenValues replacement = eigenValues |> Vector.map (fun ev -> if ev <= 0.000001 then replacement else ev )    

    let (originalEigenValues,originalEigenVectors, originalIsPSD) = decomposeMatrix matrix

    match originalIsPSD with
    | true -> matrix
    | _ ->
        let startN = 1
        let maxN = originalEigenValues.Count
        let newDiagonalOfOnes = originalEigenValues |> Vector.map (fun e -> 1.0)
        let rec setEVUntilPSD thisN = 
            let latestSmallestPositiveEigenValue = nThSmallestPosEigenValue originalEigenValues thisN
            let newPossibleEigenValues = replaceNegativeEigenValues originalEigenValues latestSmallestPositiveEigenValue
            let newPossibleCorrelMatrix = reconstructMatrixFromEigenDecomposition originalEigenVectors newPossibleEigenValues
            newPossibleCorrelMatrix.SetDiagonal(newDiagonalOfOnes)
            let (newEigenValues,newEigenVectors, newIsPSD) = decomposeMatrix newPossibleCorrelMatrix
            if newIsPSD || thisN = maxN then newPossibleCorrelMatrix else setEVUntilPSD (thisN + 1)
        setEVUntilPSD 1  

let aPSDMatrixThatsAlreadyPSD = ensureMatrixIsPSD newPossibleCorrelMatrix
let aPSDMatrixThatsPSDAfterOneIteration = ensureMatrixIsPSD testFullCorr

// The 2009 algoritm further reduces the minimum eigenvalue by a small amount until the result is back over the PSD threshold
let (latestEigenValues,latestEigenVectors, latestIsPSD) = decomposeMatrix newPossibleCorrelMatrix

// I'm going to stop there, as I'm not convinced that this is the same algorithm we use in the DB RWTS any more (the diagonals are not = 1.0, so something is up...)