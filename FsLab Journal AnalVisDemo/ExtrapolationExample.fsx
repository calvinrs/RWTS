(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**
Equity Extrapolation in F#
================

Equity Extrapolation
--------------------

We can think of the equity extrapolation process of being in 3 phases:  

1. Calibrate to ATM market volatility based on a standard starting point.
2. Evaluate the result of the first fit into one of several "cases".
3. Based on the case we determine, rerun the calibration with an adjusted set of criteria.  

The second "calibration" step may be an iterative process.  

*)

(**

TMT Diagram
-----------

This TMT layout will make more sense at the end of the document, but I will include this here to give an overview of the expected inputs and outputs of the tool.  


![](TMT_EquityExtrap_RealModels.svg)

*)



(**
<a name="Settings"></a>
Defining the Target Data
--------------------------
*)

(**
Targets here are simplified market volatilities. These will arrive in the form of a (Maturity, Strike, Value) "matrix".
We are only including what we fit to here, so only ATM Volatilities (Strike =1) are required, so we can throw away the strike information.
So, we are assuming that this is the "clean" data, where only points with a value are included (no NULL or NA values), and the data is sorted by Maturity.
*)

// targets - simplified "NOX" data for a E_NOK fit @EndMar2017
let simpleTargetVols = [  (0.25,0.18938213810157);
                            (0.5,0.192238397377696);
                            (0.75,0.194751728074236);
                            (1.0,0.19619340600353);
                            (2.0,0.192399997024925);
                            (3.0,0.191733301529111);
                            (4.0,0.189276479115615);
                            (5.0,0.18700481201725);
                            (7.0,0.179412496332435);
                            (10.0,0.169832870147339);]

(*** include-value:simpleTargetVols***)

(**
Target data will enter the tool from the **"MarketData.Equity.ImpliedVol"** model for the given equity index.  

Most commonly, this will come from the "B+H_Market" stream.  
*)

(**
<a name="Settings"></a>
Defining the Tool Settings and Parameters
--------------------------

First, the unconditional volatility limit will come from a FEA target value. This will have its own input model **TargetData.MC.EquityIVLimit**.  
*)

let UncVolLimit = 0.2208


(**
The remaining parameters can be considered part of the "Settings" of the tool.  

If we want to handle this in a purely "Model" fashion, this will come from an input **"Settings.MarketData.Equity.ExtrapolatedIV"** model.
*)


// parameter seeds

let automaticIncreaseMaxFactor = true
let maxFactorIncrease = 1.25

// unconditional targets

let weightsDecayFactor = 0.5

let seedMinFactor = 1.05
let initialSeedMaxFactor = 1.4

let seedAlpha = 0.5

(* We will need to set the bounds of the model parameters for the optimisation routine.
**)
let seedSigmaZeroLowerBound = 0.0001
let seedSigmaZeroUpperBound = 100.0 //unbounded, so "something big"

let seedAlphaLowerBound = 0.05
let seedAlphaUpperBound = 100.0 //unbounded, so "something big"

(* We define the terms that we wish to evaluate for our final answer. 
Note that the final extrapolated ATM vol curve will be replaced with the actual market values up to the maximum term of the market data.
**)
let outMaturities = [0.25;0.5;0.75] @ [1.0..10.0] @ [15.0 ;20.0; 25.0; 30.0;40.0;50.0]


(** If we choose to "automaticIncreaseMaxFactor" then we check if the maximum market data point already exceeds the maxLimit. 
If so, then we increase this by the defined maxFactorIncrease
*)

let seedMaxFactor = 
    let initialMaxLimit = initialSeedMaxFactor * UncVolLimit
    let actualFactorIncrease = if automaticIncreaseMaxFactor 
                                then if simpleTargetVols|> List.maxBy (fun (t, v) -> v) |> snd > initialMaxLimit then 1.25 * initialSeedMaxFactor else 1.0
                                else 1.0
    actualFactorIncrease * initialSeedMaxFactor

// Now we can set the final max/min limits
let maxLimit = seedMaxFactor * UncVolLimit
let minLimit = seedMinFactor * UncVolLimit

(* We can set the final values for our seeds based on the internal functions defined. 
**)
let seedSigmaInfLowerBound = minLimit
let seedSigmaInfUpperBound = maxLimit




(* Now, given all of the input settings, and having calculated the additional internal settings that are a function of other values, 
we can pass these to a "class" for the ModelParams and OptimiserParams. 
The eqivalent in F# is to create a record type for each of these "Params".  
**)

// functions
// Try with a type as the Seed input
type ModelParams = {sigmaZero: float; sigmaInf:float; alpha:float }

let modelParamSeeds = {sigmaZero = minLimit; sigmaInf = maxLimit; alpha = seedAlpha }

//If we want to optimise, then we will need to provide upper and lower bounds to the problem
let seedsLowerBound = {sigmaZero = seedSigmaZeroLowerBound; sigmaInf = seedSigmaInfLowerBound; alpha = seedAlphaLowerBound }
let seedsUpperBound = {sigmaZero = seedSigmaZeroUpperBound; sigmaInf = seedSigmaInfUpperBound; alpha = seedAlphaUpperBound }

// Combine Seeds+Bounds to create a full OptimiserParams type
type OptimiserParams = {seeds: ModelParams; lowerBound: ModelParams; upperBound: ModelParams}

let optimiserStartParams = {seeds = modelParamSeeds; lowerBound = seedsLowerBound; upperBound = seedsUpperBound}

(**
<a name="Extrapolation"></a>
Calculating the model
--------------------------

The implied volatilty (IV) at each point in time T is defined as:  

$$ IV(T) = \sqrt{\sigma_\infty^2 + \frac{(\sigma_0^2-\sigma_\infty^2)}{\alpha T}(1-\exp(-\alpha T))}  $$


This can be implemented in F# as follows:
*)

let modelImpliedVolatility modelParams t = sqrt((modelParams.sigmaInf * modelParams.sigmaInf) + (1.0 - exp(-modelParams.alpha * t)) * (modelParams.sigmaZero * modelParams.sigmaZero - modelParams.sigmaInf * modelParams.sigmaInf) / (modelParams.alpha * t))

// And, we can test this works straight away
modelImpliedVolatility modelParamSeeds 5.0 //  = 0.2832060327621


(**
Defining the weighting scheme
------------------------------
The weighting scheme for the calibration is dependent on the shape of the data. For market data with a "v-shaped" last 3 points, we use a different weighting scheme.
*)

type WeightMethod =  VShaped | Standard

// Apply weights to the Target 
let weightedTargetWithDecay decayFactor wtMethod simpleTarget  = 

    // find the final point of the market data
    let maxTerm = simpleTarget |> List.map (fun (t, v) -> t) |> List.max  
    
    // There is a complicated bit in setting the weights, where we find the slope of the curve at the final point, and remove the weight on any point that is not going in the same direction.
    let termDeltas = simpleTarget |> List.pairwise |> List.map (fun ((t1, v1),(t2, v2)) -> (t1, (v2 - v1) / (t2 - t1)) ) 
    let lastDelta = termDeltas |> List.rev |> List.head |> snd
    let secondLastDelta = termDeltas |> List.rev |> List.item 1 |> snd
    let secondLastMarketTerm = termDeltas |> List.rev |> List.head |> fst
    let isDeltaTheSameDirection lastVal thisVal = if lastVal < 0.0 then thisVal < 0.0 else thisVal > 0.0
    let ignoreSlopeMap = termDeltas |> List.map (fun (t, v) -> (t, isDeltaTheSameDirection lastDelta v)) |> Map.ofList
    let fullIgnoreSlopeMap = ignoreSlopeMap.Add(maxTerm, true)

    // also we check if there is a V present at the end of the series  
    let isVPresent = lastDelta > 0.0 && secondLastDelta < 0.0

    // We can now find the exponentially decaying weights for all market points.
    let extrapWeightsTargetVols  = simpleTarget |> List.map (fun (t, v) -> (t, v, exp(-decayFactor * (maxTerm - t))))

    // Finally, we adjust depending on the method chosen. 
    // For the standard case, we ignore the points with a change in direction before it
    // For the V case, we ignore the penultimate point only
    let finalExtrapWeightsTargetVols = 
        match wtMethod with
        | Standard -> extrapWeightsTargetVols |> List.map (fun (t, v, w) -> (t, v, if fullIgnoreSlopeMap.[t] then w else 0.0))
        | VShaped -> extrapWeightsTargetVols |> List.map (fun (t, v, w) -> (t, v, if (t = secondLastMarketTerm) && isVPresent  then 0.0 else w ))
    
    finalExtrapWeightsTargetVols

// We can "bake in" the decay factor, as this will rarely change
let weightedTarget = weightedTargetWithDecay weightsDecayFactor 

// Likewise, we can bake in the Method
let standardWeightedTarget = weightedTarget WeightMethod.Standard
let vShapedWeightedTarget = weightedTarget WeightMethod.VShaped

// Finally, we can feed in the target IV data to produce the weighted version of the target
let finalWeightedTarget = standardWeightedTarget simpleTargetVols


// Test cases for the V-shaped data

// no v-shape, so no weights removed
vShapedWeightedTarget simpleTargetVols


let testVShapedVols =  [  (0.25,0.18938213810157);
                            (0.5,0.192238397377696);
                            (0.75,0.194751728074236);
                            (1.0,0.19619340600353);
                            (2.0,0.192399997024925);
                            (3.0,0.191733301529111);
                            (4.0,0.189276479115615);
                            (5.0,0.190);
                            (7.0,0.180);
                            (10.0,0.20);]

// Should be unweighted below the last 2 points
standardWeightedTarget testVShapedVols

// should have the penultimate point zeroed out
vShapedWeightedTarget testVShapedVols

(**
Weighted Squared Error
======================

To solve the problem, we need to produce a function that returns a weighted squared error for the problem posed.  

We can then use an optimiser to find a model parameterisation that minimises the value of this function.  

The weighting schemes shown below differ slightly from the ones in the excel tool. I believe that we have a weighted squared error, so this should be defined as:  

$ {WeightedMSE}=\sum _{{i=1}}^{n}w_ie_i^{2} $

Whereas in the tool we have:  

$ {ToolMSE}={\frac  {1}{n}}\sum _{{i=1}}^{n}w_ie_i^{2} $

I think at some point we have changed this to a weighted error, but forgot to remove the $ {\frac {1}{n}} $ from the MSE formula.   
*)

//TODO turn this into a function that returns "extrapSqdError" given the model parameters only
let errorToMinimise modelParams target = 

    let weightedMktModelPairs = target |> List.map (fun (t, v, w) -> (v,  modelImpliedVolatility modelParams t, w))
    // required if we need to divide by N
    let numWeightedPoints = weightedMktModelPairs |> List.map (fun (mkt, mdl, weight) -> if weight = 0.0 then 0.0 else 1.0) |> List.sum
    let mktModelErrors = weightedMktModelPairs |> List.map (fun (mkt, mdl, weight) -> weight * (mkt - mdl) * (mkt - mdl) )
    // to replicate the excel tool exactly, then the output is List.sum mktModelErrors / numWeightedPoints
    // otherwise, it is the sum of squared residuals multiplied by the weights
    List.sum mktModelErrors 



//To use an optimiser, we need to turn this into a function that returns "extrapSqdError" given the model parameters only.
// i.e. we have a function that we want to minimise in value, by changing its input parameter
let extrapSqdError modelParams = finalWeightedTarget |> errorToMinimise modelParams 

// Calculate the Error, based on the starting modelParams
extrapSqdError modelParamSeeds

(**
Phase 1 - Initial Calibration
-------------------

Now that we have:

1) A bounded set of parameters
2) A function that returns the squared error for the problem ("fitting model to market data")

We have all the ingredients to pass this problem to an optimiser, to return the parameter set that minimises the error function.  


> Your mission is to wire in this model to an existing optimiser. The standard in our common libraries will be the "Levenberg-Marquart" optimiser. 
> Note that any settings for this optimiser will need to be added to the set of tool settings, or the input settings model (or its own input model).

We can pretend for now that we have a function that serves as an optimiser - here, it just returns hard-coded solution, rather than the expected "ModelParams" type containing the solution.
*)

let solveProblem (errorFunc: ModelParams-> float) (modelParameters: OptimiserParams) = {sigmaZero = 0.158840464328897; sigmaInf = 0.23184; alpha = 0.05 }

// and we can curry this for the Equity Extrapolation problem, as the error function will not change

let solveExtrapSqdError = solveProblem extrapSqdError

(**
For the final "output" extrapolated ATM volatility, we replace the model value with the actual market value, when that value exists.
*)

let modelValuesAtMats modelParams marketPoints maturities = 
    let maxTerm = marketPoints |> List.map (fun (t, v) -> t) |> List.max  
    let newValues = maturities |> List.filter (fun t -> t > maxTerm)  |> List.map (fun (t) -> (t,  modelImpliedVolatility modelParams t))
    marketPoints @ newValues

let modelValues modelParams  = modelValuesAtMats modelParams simpleTargetVols outMaturities

let extrapModel = modelValues modelParamSeeds

(**
We can visualise the initial "fit" to the target vols (we will be nowhere near at first, as we are starting from the seed values).
*)

open XPlot.GoogleCharts

let plotResult model = 
    let options =
        Options(
            title = "Equity Extrapolation",
            vAxis =
                Axis(
                    title = "Implied Volatility"               
                ),
            hAxis =
                Axis(
                    title = "Option Maturity"               
                )

        )
 

    [model; simpleTargetVols] 
    |> Chart.Scatter 
    |> Chart.WithOptions options
    |> Chart.WithLabels ["Extrapolated Model"; "Market"]

// plot the evaluated seeds
(*** define-output:plot1 ***)
plotResult extrapModel
(*** include-it:plot1 ***)


(**
We can compare this to the "solution" for the calibration, as taken from the first pass calibration in the Excel tool.  
*)

(*** define-output:plot2 ***)
// show the initial solution, before we assess the cases
let initialSolution = {sigmaZero = 0.158840464328897; sigmaInf = 0.23184; alpha = 0.05 }

// show results for final answer
let solution = {sigmaZero = 0.158840464328897; sigmaInf = 0.23184; alpha = 0.05 }
solution |> modelValues |> plotResult
(*** include-it:plot2 ***)

// Calculate the Error, based on the solution
extrapSqdError solution

(**
---
*)

(**
<a name="refitConditions"></a>
Phase 2 - Evaluating the optimised solution
--------------------------
To evaluate the solution, we need to evaluate properties about the fit. 

Here we will build up a number of predicate cases from smaller test predicates that we will test the initial solution against, to see which "case", if any, we have discovered.  

We then can decide if the case dictates that we need to try a further optimisation.  

*)

(**
We can define our rounding functions we will use as follows, and test that they behave as expected.

> NOTE: I believe this has been changed to a "nearest10" in a later spec, but this is to be confirmed.

*)
let nearest5(percentage) = round(20.0 * percentage) / 20.0
let roundUpToNearest5 percentage = ceil(20.0 * percentage) / 20.0
let roundDownToNearest5 percentage = floor(20.0 * percentage) / 20.0

// Testing these rounding functions
let testPercentages = [0.7;0.72;0.725;0.73;1.0;1.02;1.025;1.03;1.07;1.08;1.3]
let testRoundDown  = List.map roundDownToNearest5 testPercentages
let testRoundUp  = List.map roundUpToNearest5 testPercentages

(**
Some properties will be described as small, i.e. less than a threshold. 
If a number is NOT small, by this definition it is large.
*)
// this is our definition of a small number - we can use this as an "equals, within tolerance"
let tolerance = 0.00001
let withinTolerance x t =  abs x < t
let isSmall x =  withinTolerance x tolerance


(**
We will test the gradient between the market data points.
*)
let termDeltas = simpleTargetVols |> List.pairwise |> List.map (fun ((t1, v1),(t2, v2)) -> (t1, (v2 - v1) / (t2 - t1)) ) 

let isLastMarketGradientSmall = termDeltas |> List.rev |> List.head |> snd |> isSmall
let isPenultimateMarketGradientSmall = termDeltas |> List.rev |> List.item 2 |> snd |> isSmall
let isLastMarketGradientPositive = termDeltas |> List.rev |> List.head |> snd > 0.0
let isLastMarketGradientStrictlyPositive = termDeltas |> List.rev |> List.head |> snd > -0.0015 //can't see this number in the spec


let areFinalMarketPointsSlopingUpward = termDeltas |> List.rev |> List.head |> snd > 0.0
let areFinalMarketPointsSlopingDownward = not areFinalMarketPointsSlopingUpward
(**
Analyse the market data for a V-shape
*)
let lastMarketPoint = simpleTargetVols |> List.rev |> List.head 
let secondLastMarketPoint = simpleTargetVols |> List.rev |> List.skip 1 |> List.head 
let thirdLastMarketPoint = simpleTargetVols |> List.rev |> List.skip 2 |> List.head 

let isVPresent = (snd thirdLastMarketPoint - snd secondLastMarketPoint) > tolerance && (snd secondLastMarketPoint - snd lastMarketPoint) < - tolerance
let isMarketThirdLastLargerThanSecondLast  = (snd thirdLastMarketPoint - snd secondLastMarketPoint)  > tolerance

(*** hide ***)
(*
let threePointsInAnUpVShape = [false;true]
let threePointsInADownVShape = [true;false]
let theLastTwoGradients = termDeltas |> List.rev |> List.take 2 |> List.rev |> List.map (fun (t,v) -> v > 0.0)
let doTheLastMarketPointsFormAVShape = 0 = List.compareWith (fun b1 b2 -> if b1 = b2 then 0 else 1) theLastTwoGradients threePointsInADownVShape

//theLastTwoGradients |> List.pairwise  |> List.forall (fun (b1, b2) -> b1 = b2)
**)

(**
Short-end behavior
*)
let oneYearMarketAverage = simpleTargetVols |> List.filter (fun (t,v) -> t <= 1.0) |> List.averageBy (fun (t,v) -> v)

(**
Long-end behavior
*)

let isLastMarketDataPointBelowStartingMin = (snd lastMarketPoint - minLimit) < -tolerance
let isLastMarketDataPointAboveStartingMin = not isLastMarketDataPointBelowStartingMin
let isLastMarketDataPointLessThanSigmaInf = (snd lastMarketPoint - solution.sigmaInf) < tolerance
let isLastMarketDataPointGreaterThanSigmaInf = not isLastMarketDataPointLessThanSigmaInf

let isAlphaOnTheLowerBound = isSmall (solution.alpha - seedAlphaLowerBound)
let isSigmaInfAtTheMinLimit = isSmall (solution.sigmaInf - minLimit)
let isSigmaInfAtTheMaxLimit = isSmall (solution.sigmaInf - maxLimit)

let isAlphaLargerThanTheLB = (solution.alpha - seedAlphaLowerBound) > tolerance
let isSigmaInfAboveTheMinLimit = (solution.sigmaInf - minLimit) > tolerance // does the "Max Limit" move?
let isSigmaInfAboveTheMaxLimit = (solution.sigmaInf - maxLimit) > tolerance // does the "Max Limit" move?
let isSigmaInfBelowTheMinLimit = not isSigmaInfAboveTheMinLimit
let isSigmaInfBelowTheMaxLimit = not isSigmaInfAboveTheMaxLimit

let isLastMarketGradientLarge = not isLastMarketGradientSmall
let isPenultimateMarketGradientLarge = not isPenultimateMarketGradientSmall

let weSayTheMarketIsUpwardSloping = (oneYearMarketAverage - snd lastMarketPoint) < tolerance
let weSayTheMarketIsDownwardSloping = not weSayTheMarketIsUpwardSloping

(**
We can now try to evaluate what "case" we have produced, and decide if there is to be any adjustments and refitting done.

> There is an orignial case, where if the extrapolated value exceeds the max limit, we add the max limit as a target point and weight it.
> Is this even possible, given the constraints?

Otherwise, we can identify the cases as specified in the documentation and revised excel tool code.
*)



let isCase11 = (isLastMarketGradientSmall && isLastMarketGradientPositive) || isPenultimateMarketGradientSmall

let isCase12 = isLastMarketDataPointBelowStartingMin && isAlphaLargerThanTheLB && isSigmaInfAboveTheMinLimit && isVPresent && isLastMarketGradientLarge && isPenultimateMarketGradientLarge

let isCase13 = areFinalMarketPointsSlopingUpward && isLastMarketDataPointBelowStartingMin && isAlphaOnTheLowerBound && isSigmaInfAtTheMinLimit && isLastMarketGradientLarge && isPenultimateMarketGradientLarge

let isCase2 = areFinalMarketPointsSlopingUpward && isLastMarketDataPointLessThanSigmaInf && isSigmaInfAtTheMaxLimit && isLastMarketDataPointAboveStartingMin  && isLastMarketGradientLarge && isPenultimateMarketGradientLarge

let isCase321 = areFinalMarketPointsSlopingDownward && weSayTheMarketIsDownwardSloping && isLastMarketDataPointLessThanSigmaInf
let isCase322 = areFinalMarketPointsSlopingDownward && weSayTheMarketIsUpwardSloping && isLastMarketDataPointGreaterThanSigmaInf

let isCase311 = areFinalMarketPointsSlopingDownward  && weSayTheMarketIsUpwardSloping && isLastMarketDataPointLessThanSigmaInf && isMarketThirdLastLargerThanSecondLast
let isCase312 = areFinalMarketPointsSlopingDownward  && weSayTheMarketIsUpwardSloping && isLastMarketDataPointLessThanSigmaInf && isSigmaInfBelowTheMinLimit && isLastMarketGradientStrictlyPositive

//Final case is if we have not reached any other case, plus the extra condition "isSigmaInfAtTheMinLimit"
//Removed in the latest spec, we no longer requires this
let isCase14 = List.forall (not) [isCase11;isCase12;isCase13;isCase2;isCase311;isCase312;isCase321;isCase322] && isSigmaInfAtTheMinLimit

let caseList = ["Case11";"Case12";"Case13";"Case2";"Case311";"Case312";"Case321";"Case322";"noAdjustmentCase"]
let isCaseList = [isCase11;isCase12;isCase13;isCase2;isCase311;isCase312;isCase321;isCase322;true]

let caseMap = List.zip caseList isCaseList

let thisCaseIs = caseMap |> List.find (fun (k, v) -> v) |> fst

(**
If we return the "noAdjustmentCase", we can return the initial solution as the final result.
*)

(**
3. Checking Case and refitting
---------------------------

Now that we have run the initial optimisation, and checked this result to see which state we are in, we will then see what further optimisation(s) we will need to do to get to the final result.  

So, we will need something like a switch statement to test what case we have returned, and what actions to take for this case.
*)

let revisedStartParams = { seedsLowerBound with sigmaInf = 0.1 }

let doTheCase caseIs originalSolution = 
    match caseIs with
    | "Case11" -> 
        // adjust the min factor
        let revisedSeedMinFactor = roundUpToNearest5 (snd lastMarketPoint / UncVolLimit ) - 0.05
        let revisedStartParams = {seeds = modelParamSeeds; lowerBound = { seedsLowerBound with sigmaInf = revisedSeedMinFactor }; upperBound = seedsUpperBound}
        solveExtrapSqdError revisedStartParams 
    // and so on...
    // with some cases being recursive   
    | "noAdjustmentCase" -> originalSolution
    | _ -> originalSolution         

(**
Output Requirements
---------------------------

We will now have a final set of parameter values, and we can evaluate a new "ExtrapModel" curve from it.

We will want to record the final fit and the corresponding values as our key model output. 
We will also want to see what start parameters we ended up with - most importantly, what bounds we used.

We can also return the initial fit, and return information about the "case" we returned from it. 
We can also export the initial bounds as they were evaluated, so we can compare with the final bounds.  

For a full summary of what the export should look like, please see *U:\\CS\\2017 Projects\\Equity Extrapolation\\Export Mockups\\Mock_EquityExtrap_NewTooExport.xlsm*

*)