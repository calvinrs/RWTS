(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**
FsLab Experiment
================
*)





// unconditional targets
let BE_Limit = 0.2208
let weights_decayfactor = 0.5

// parameter seeds
let seed_minFactor = 1.05
let seed_maxFactor = 1.4

let seed_sigmaZero = seed_minFactor * BE_Limit
let seed_sigmaInf = seed_maxFactor * BE_Limit
let seed_alpha = 0.5

// required output terms
let outMaturities = [0.25;0.5;0.75] @ [1.0..10.0] @ [15.0 ;20.0; 25.0; 30.0;40.0;50.0]


// targets

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



// functions



let imp_vol sigmaZero sigmaInf alpha t = sqrt((sigmaInf * sigmaInf) + (1.0 - exp(-alpha * t)) * (sigmaZero * sigmaZero - sigmaInf * sigmaInf) / (alpha * t))
//let imp_vol sigmaZero sigmaInf alpha t = sqrt(sigmainf * sigmainf + (1 - exp(-alpha * t)) * (sigmaZero * sigmaZero - sigmaInf * sigmaInf) / (alpha * t)


//test this

imp_vol seed_sigmaZero seed_sigmaInf seed_alpha 0.25 // = 0.237185735276719
imp_vol seed_sigmaZero seed_sigmaInf seed_alpha 5.0 //  = 0.2832060327621


let params_seed = (seed_sigmaZero, seed_sigmaInf, seed_alpha)

let inline imp_vol' (sigmaZero, sigmaInf, alpha) t = sqrt((sigmaInf * sigmaInf) + (1.0 - exp(-alpha * t)) * (sigmaZero * sigmaZero - sigmaInf * sigmaInf) / (alpha * t))

imp_vol' params_seed 5.0 //  = 0.2832060327621

let ExtrapImpVol = List.map (fun t -> imp_vol' params_seed t) Maturities


// Try with a type as the Seed input
type ExtrapParams = {sigmaZero: float; sigmaInf:float; alpha:float }
let seeds_as_type = {sigmaZero = seed_sigmaZero; sigmaInf = seed_sigmaInf; alpha = seed_alpha }

let imp_vol'' seeds t = sqrt((seeds.sigmaInf * seeds.sigmaInf) + (1.0 - exp(-seeds.alpha * t)) * (seeds.sigmaZero * seeds.sigmaZero - seeds.sigmaInf * seeds.sigmaInf) / (seeds.alpha * t))

imp_vol'' seeds_as_type 5.0 //  = 0.2832060327621


// Calculate the Error, based on the starting seeds

// find the final point of the market data
let maxTerm = simpleTargetVols |> List.map (fun (t, v) -> t) |> List.max


// There is a complicated bit in setting the weights, where we find the slope of the curve at the final point, and remove the weight on any point that is not going in the same direction.
let termDeltas = simpleTargetVols |> List.pairwise |> List.map (fun ((t1, v1),(t2, v2)) -> (t1, (v2 - v1) / (t2 - t1)) ) 

let lastDelta = termDeltas |> List.rev |> List.head |> snd

let isDeltaTheSameDirection lastVal thisVal = if lastVal < 0.0 then thisVal < 0.0 else thisVal > 0.0

// test
isDeltaTheSameDirection lastDelta -1.0
isDeltaTheSameDirection lastDelta 1.0

let ignoreSlopeMap = termDeltas |> List.map (fun (t, v) -> (t, isDeltaTheSameDirection lastDelta v)) |> Map.ofList
let fullIgnoreSlopeMap = ignoreSlopeMap.Add(maxTerm, true)


// We can now find the exponentially decaying weights for all market points, then ignore the points with a change in direction before it.
let extrapWeightsTargetVols  = simpleTargetVols |> List.map (fun (t, v) -> (t, v, exp(-weights_decayfactor * (maxTerm - t))))

let finalExtrapWeightsTargetVols = extrapWeightsTargetVols |> List.map (fun (t, v, w) -> (t, v, if fullIgnoreSlopeMap.[t] then w else 0.0))

let mktModelPairs = List.map (fun (t, v) -> (v,  imp_vol'' seeds_as_type t)) simpleTargetVols
let weightedMktModelPairs = finalExtrapWeightsTargetVols |> List.map (fun (t, v, w) -> (v,  imp_vol'' seeds_as_type t, w))

let numWeightedPoints = weightedMktModelPairs |> List.map (fun (mkt, mdl, weight) -> if weight = 0.0 then 0.0 else 1.0) |> List.sum

let mktModelErrors = weightedMktModelPairs |> List.map (fun (mkt, mdl, weight) -> weight * (mkt - mdl) * (mkt - mdl) )

let extrapSqdError = List.sum mktModelErrors / numWeightedPoints

//TODO turn this into a function that returns "extrapSqdError" given the model parameters only

//let errorToMinimise modelParams = 


// We can plot the market points Vs the model values

let extrapModel = outMaturities |> List.map (fun (t) -> (t,  imp_vol'' seeds_as_type t))


open XPlot.GoogleCharts

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
 

[extrapModel; simpleTargetVols] 
|> Chart.Scatter 
|> Chart.WithOptions options
|> Chart.WithLabels ["Extrapolated Model"; "Market"]