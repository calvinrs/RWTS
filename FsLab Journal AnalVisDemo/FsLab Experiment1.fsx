(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**
FsLab Experiment
================
*)

open System
open System.IO
open FSharp.Data
open Deedle
open FSharp.Charting

#r "MathNet.Numerics.dll"
open MathNet.Numerics.Distributions

let randomPrice seed drift volatility initial start span count = 
  (*[omit:(Implementation omitted)]*) 
  let dist = Normal(0.0, 1.0, RandomSource=Random(seed))  
  let dt = (span:TimeSpan).TotalDays / 250.0
  let driftExp = (drift - 0.5 * pown volatility 2) * dt
  let randExp = volatility * (sqrt dt)
  ((start:DateTimeOffset), initial) |> Seq.unfold (fun (dt, price) ->
    let price = price * exp (driftExp + randExp * dist.Sample()) 
    Some((dt, price), (dt + span, price))) |> Seq.take count(*[/omit]*)

let today = DateTimeOffset(DateTime.Today)

(**
Partially apply the function, so now we only need span, count to get the sequence
*)
let stock1 = randomPrice 1 0.1 3.0 20.0 today
let stock2 = randomPrice 2 0.2 1.5 22.0 today

Chart.Combine
 [ stock1 (TimeSpan(0,1,0)) 1000 |> Chart.FastLine
   stock2 (TimeSpan(0,1,0)) 1000 |> Chart.FastLine ]

(**
Create series over overlapping observations, so we can show how to zip series together using a DateTime-y key
*)
let s1 = stock1 (TimeSpan(1,0,0)) 6 |> series

let s2 = stock2 (TimeSpan(0,30,0)) 12 |> series

let s3 = stock1 (TimeSpan(1,5,0)) 6 |> series

s1.Zip(s2, JoinKind.Left)
s1.Zip(s2, JoinKind.Right)

// Use left series key and find the nearest previous
// (smaller) value from the right series
s1.Zip(s3, JoinKind.Left, Lookup.ExactOrSmaller)

(**
More powerful way is to use data frames
*)

let f1 = Frame.ofColumns ["S1" => s1]
let f2 = Frame.ofColumns ["S2" => s2]
let f3 = Frame.ofColumns ["S3" => s3]

f1.Join(f2, JoinKind.Outer)
f2.Join(f3, JoinKind.Inner)

f2.Join(f3, JoinKind.Left, Lookup.ExactOrSmaller)
f2.Join(f3, JoinKind.Left, Lookup.Exact)

(**
Same operations as above, but using a function syntax
*)

Frame.join JoinKind.Outer f1 f2

Frame.joinAlign JoinKind.Left Lookup.ExactOrSmaller f2 f3


(**
Windowing, chunking and pairwise
----------------------------------
*)

let lf = stock1 (TimeSpan(0,1,0)) 6 |> series

// Create series of series representing individual windows
lf |> Series.window 4
// Aggregate on sub-series
lf |> Series.windowInto 4 Stats.mean
lf |> Series.windowInto 4 Series.firstValue

//can join back the aggregate functions to the original series

let lfm1 = lf |> Series.windowInto 4 Stats.mean
Frame.ofColumns [ "Orig" => lf; "Means" => lfm1]

// we can use smaller windows to cover for periods where the data does not exist
let lfm2 =
  lf |> Series.windowSizeInto (4, Boundary.AtBeginning) (fun ds -> Stats.mean ds.Data)

Frame.ofColumns [ "Orig" => lf; "Means" => lfm2 ]

//series with characters, to demonstrate boundary setting keys at the beginning of a range
let st = Series.ofValues [ 'a' .. 'e' ]
st |> Series.windowSizeInto (3, Boundary.AtEnding) (function 
  | DataSegment.Complete(ser) -> 
        String(ser |> Series.values |> Array.ofSeq).ToUpper()
  | DataSegment.Incomplete(ser) -> 
        String(ser |> Series.values |> Array.ofSeq).PadRight(3, '-') )

//windows can be over a distance between keys, rather than a count
let hourly = stock1 (TimeSpan(1, 0, 0)) (30*24) |> series
Chart.Line(hourly)

hourly |> Series.windowDist (TimeSpan(24, 0, 0))

hourly |> Series.windowWhile (fun d1 d2 -> d1.Date = d2.Date)

//CHUNKING (windows with no overlaps)

let hf = stock1 (TimeSpan(0, 0, 1)) 600 |> series

hf |> Series.chunkSize (10, Boundary.AtEnding)

hf |> Series.chunkDistInto (TimeSpan(0, 0, 10)) Series.firstValue

hf |> Series.chunkWhile (fun k1 k2 -> (k1.Hour, k1.Minute) = (k2.Hour, k2.Minute))

//Pairwise

hf |> Series.pairwise

//can use this to take log differences
hf |> Series.pairwiseWith (fun k (v1, v2) -> log v2 - log v1)

// need to use window functions if we want to populate something for the first key
hf |> Series.windowSizeInto (2, Boundary.AtBeginning) (function 
  | DataSegment.Incomplete s -> s.GetAt(0)
  | DataSegment.Complete s -> s.GetAt(1) - s.GetAt(0) )


// SAMPLING AND RESAMPLING

// Generate a bit less than 24 hours of data with 13.7sec offsets
let mf = stock1 (TimeSpan.FromSeconds(13.7)) 6300 |> series

// Generate keys for all minutes in 24 hours
let keys = [ for m in 0.0 .. 24.0*60.0-1.0 -> today.AddMinutes(m) ]

mf |> Series.lookupAll keys Lookup.ExactOrGreater
mf |> Series.lookupAll keys Lookup.ExactOrSmaller

//resampling

// For each key, collect values for greater keys until the 
// next one (chunk for 11:59:00 PM is empty)

mf |> Series.resample keys Direction.Forward

mf |> Series.resample keys Direction.Backward

// can aggregate these chunks (list is models own interpretation of adding new columns)

mf |> Series.resampleInto keys Direction.Backward (fun k s -> [Stats.mean s, Stats.stdDev s])

//member syntax
mf.Resample(keys, Direction.Forward)

// Generate 2.5 months of data in 1.7 hour offsets
let ds = stock1 (TimeSpan.FromHours(1.7)) 1000 |> series

// sample by day (of type 'DataTimme')
ds |> Series.resampleEquiv (fun d -> d.Date)
//or
ds.ResampleEquivalence(fun d -> d.Date)

// Uniform resampling

// Create input data with non-uniformly distributed keys
// (1 value for 10/3, three for 10/4 and two for 10/6)
let days =
  [ "10/3/2013 12:00:00"; "10/4/2013 15:00:00" 
    "10/4/2013 18:00:00"; "10/4/2013 19:00:00"
    "10/6/2013 15:00:00"; "10/6/2013 21:00:00" ]

let nu = stock1 (TimeSpan(24, 0, 0)) 10 |> series |> Series.indexWith days |> Series.mapKeys DateTimeOffset.Parse

// Generate uniform resampling based on dates. Fill
// missing chunks with nearest smaller observations
let sampled = nu |> Series.resampleUniform Lookup.ExactOrSmaller (fun dt -> dt.Date) (fun dt -> dt.AddDays(1.0))

// Turn into frame with multiple columns for each day
// (to format the result in a readable way)
sampled
|> Series.mapValues Series.indexOrdinally
|> Frame.ofRows

// Sampling time seires

// Generate 1k observations with 1.7 hour offsets
let pr = stock1 (TimeSpan.FromHours(1.7)) 1000 |> series

// Sample at 2 hour intervals; 'Backward' specifies that
// we collect all previous values into a chunk.
pr |> Series.sampleTime (TimeSpan(2, 0, 0)) Direction.Backward

// Same thing using member syntax - 'Backward' is the dafult
pr.Sample(TimeSpan(2, 0, 0))

// Get the most recent value, sampled at 2 hour intervals
pr |> Series.sampleTimeInto (TimeSpan(2, 0, 0)) Direction.Backward Series.lastValue

// CALCULATIONS AND STATISTICS

let sample = stock1 (TimeSpan.FromHours(1.7)) 6 |> series

// Calculates: new[i] = s[i] - s[i-1]
let diff1 = sample |> Series.diff 1
// Diff in the opposite direction
let diffM1 = sample |> Series.diff -1

// Shift series values by 1
let shift1 = sample |> Series.shift 1

// Align all results in a frame to see the results
let df = 
  [ "Shift +1" => shift1
    "Diff +1" => diff1
    "Diff" => sample - shift1
    "Orig" => sample ] |> Frame.ofColumns

// operators and functions

// Subtract previous value from the current value
sample - sample.Shift(1)


// Calculate logarithm of such differences
log (sample - sample.Shift(1))

// calculate log differences - CS
log(sample) - log(sample.Shift(1))

// Calculate square of differences
sample.Diff(1) ** 2.0

// Calculate average of value and two immediate neighbors
(sample.Shift(-1) + sample + sample.Shift(1)) / 3.0

// get absolute value of differences
abs (sample - sample.Shift(1))

// Get absolute value of distance from the mean
abs (sample - (Stats.mean sample))


// custom functions

// Truncate value to interval [-1.0, +1.0]
let adjust v = min 1.0 (max -1.0 v)

// Apply adjustment to all function
adjust $ sample.Diff(1)

// The $ operator is a shorthand for
sample.Diff(1) |> Series.mapValues adjust

/// Multiply all numeric columns by a given constant

df * 0.65

// apply function to all columns in a series

let conv x = min x 20.0

df |> Frame.mapRowValues (fun os -> conv $ os.As<float>())
   |> Frame.ofRows

// Sum each column and divide results by a constant
Stats.sum df / 6.0

// Divide sum by mean of each frame column
Stats.sum df / Stats.mean df


(**
Working with data frames in F# 
------------------------------

(http://bluemountaincapital.github.io/Deedle/frame.html)

*)

let root = __SOURCE_DIRECTORY__ + "/data/"

// Assuming 'root' is a directory containing the file
let titanic = Frame.ReadCsv(root + "Titanic.csv")

// Read data and set the index column & order rows
let msft = 
  Frame.ReadCsv(root + "stocks/msft.csv")
  |> Frame.indexRowsDate "Date"
  |> Frame.sortRowsByKey

// Specify column separator
let air = Frame.ReadCsv(root + "AirQuality.csv", separators=";")

// simpler syntax for reading csv and indexing by date
let msftsimpler = 
  Frame.ReadCsv<DateTime>(root + "stocks/msft.csv", indexCol="Date")
  |> Frame.sortRowsByKey

// Save CSV with semicolon separator
air.SaveCsv(Path.GetTempFileName(), separator=';')

// Save as CSV and include row key as "Date" column
msft.SaveCsv(Path.GetTempFileName(), keyNames=["Date"], separator='\t')

// Loading F# records or .NET objects

type Person =
  {Name:string; Age:int; Countries: string list;}

let peopleRecords = 
  [ { Name = "Joe"; Age = 51; Countries = [ "UK"; "US"; "UK"] }
    { Name = "Tomas"; Age = 28; Countries = [ "CZ"; "UK"; "US"; "CZ" ] }
    { Name = "Eve"; Age = 2; Countries = [ "FR" ] }
    { Name = "Suzanne"; Age = 15; Countries = [ "US" ] } ]  
    
// Turn the list of records into data frame 
let peopleList = Frame.ofRecords peopleRecords  
// Use the 'Name' column as a key (of type string)
let people = peopleList |> Frame.indexRowsString "Name"

//Numerical columns can be accessed with the ? operator
people?Age
//Non-numeric needs to be explicitly called with "GetColumn"
people.GetColumn<string list>("Countries")

//loading from F# Data providers
let wb = WorldBankData.GetDataContext()

/// Given a region, load GDP in current LCU and return data as 
/// a frame with two-level column key (region and country name)
let loadRegion (region: WorldBankData.ServiceTypes.Region) =
  [ for country in region.Countries ->
    (region.Name, country.Name) => 
    Series.ofObservations country.Indicators.``GDP (current LCU)`` ]
  |> frame

let eu = loadRegion wb.Regions.``Euro area``
let oecd = loadRegion wb.Regions.``OECD members``

//join and convert to billons
let world = eu.Join(oecd) / 1e9

// Expanding objects in columns

// create frame with single column "people"
let peopleNested = ["People" => Series.ofValues peopleRecords ] |> frame
// expand the people column
peopleNested |> Frame.expandCols ["People"]

// Series that contains dictionaries, containing tuples
let tuples = 
  [ dict ["A", box 1; "C", box(2,3)]
    dict ["B", box 1; "C", box(3,4)] ]
  |> Series.ofValues

// Expand dictionary keys (level 1) and tuple items (level 2)
frame ["Tuples" => tuples]
|> Frame.expandAllCols 2

// MANIPULATING DATA FRAMES

// Frame is a frame of Series!

// Get the 'Age' column as a series of 'float' values
// (the '?' operator converts values automatically)
people?Age
// Get the 'Countries' column as a series of 'string list' values
people.GetColumn<string list>("Countries")
// Get all frame columns as a series of series
people.Columns

// Get Series<string, float> 
let numAges = people?Age
// Get value using question mark
numAges?Eve
// Get value using 'Get' method
numAges.Get("Tomas")
// Returns missing when key is not found
numAges.TryGet("Calvin")

// Get column as an object series
people.Columns?Age
people.Columns?Countries

// Get column & try get column using members
people.Columns.Get("Countries")
people.Columns.TryGet("CreditCard")
// Get column at a specified offset
people.Columns.GetAt(0)
people.Columns.GetAt(1)

// Get column as object series and convert it
// to a typed Series<string, string>
people.Columns?Age.As<int>()
// Try converting column to Series<string, string>
people.Columns?Age.TryAs<string>()

// Iterate over rows and get the length of country list
people.Rows |> Series.mapValues (fun row -> row.GetAs<string list>("Countries").Length)

//typed access to rows

/// Expected columns & their types in a row
type IPerson = 
    abstract Age : int
    abstract Countries : string list

// Get rows as series of 'IPerson' values
let rows = people.GetRowsAs<IPerson>()
rows.["Tomas"].Countries

/// Alternative that lets us handle missing 'Age' values
type IPersonOpt = 
    abstract Age : OptionalValue<int>
    abstract Countries: string list

// Adding rows and columns
let more = series [ "John" => 48.0 ]
people?Age.Merge(more)

// Limited mutation of data frames
// calculate age + 1 for all people
let add1 = people?Age |> Series.mapValues ((+) 1.0)
// Add as a new series to the frame
people?AgePlusOne <- add1
// Add new series from a list of values
people?Siblings <- [0; 2; 1; 3]

// Replace existing series with new values
// (Equivalent to people?Siblings <- ...)
people.ReplaceColumn("Siblings", [3; 2; 1; 0])

// Create new object series with values for required columns
let newRow = 
  [ "Name" => box "Jim"; "Age" => box 51; 
    "Countries" => box ["US"]; "Siblings" => box 5 ]
  |> series

// Create a new data frame, containing the new series
people.Merge("Jim", newRow)

// Another option is to use mutable SeriesBuilder
let otherRow = SeriesBuilder<string>()
otherRow?Name <- "Jim"
otherRow?Age <- 51
otherRow?Countries <- ["US"]
otherRow?Siblings <- 5

// The Series property returns the built series
people.Merge("Jim", otherRow.Series)

// ADVANCED SLICING AND LOOKUP

// Sample series with different keys & values
let nums = series [ 1 => 10.0; 2 => 20.0 ]
let strs = series [ "en" => "Hi"; "cz" => "Ahoj" ]

// Lookup values using keys
nums.[1]
strs.["en"]
// Supported when key is string
strs?en

// Get an unordered sample series 
let ages = people?Age

// Returns value for a given key
ages.["Tomas"]
ages.[ ["Tomas"; "Joe"] ]

// Fails when key is not present
try ages |> Series.get "John" with _ -> nan
// Returns 'None' when key is not present
ages |> Series.tryGet "John"
// Returns series with missing value for 'John'
// (equivalent to 'ages.[ ["Tomas"; "John"] ]')
ages |> Series.getAll [ "Tomas"; "John" ]

// Get all observations as a sequence of 'KeyValuePair'
ages.Observations
// Get all observations as a sequence of tuples
ages |> Series.observations
// Get all observations, with 'None' for missing values
ages |> Series.observationsAll

// Get series with opening prices
let opens = msft?Open

// Fails. The key is not available in the series
try opens.[DateTime(2013, 1, 1)] with e -> nan
// Works. Find value for the nearest greater key
opens.Get(DateTime(2013, 1, 1), Lookup.ExactOrGreater)
// Works. Find value for the nearest smaler key
opens.Get(DateTime(2013, 1, 1), Lookup.ExactOrSmaller)

// Find value for the nearest greater key
opens |> Series.lookup (DateTime(2013, 1, 1)) Lookup.ExactOrGreater

// Get first price for each month in 2012
let dates = [ for m in 1 .. 12 -> DateTime(2012, m, 1) ]
opens |> Series.lookupAll dates Lookup.ExactOrGreater

// With ordered series, we can use slicing to get a sub-range of a series
opens.[DateTime(2013, 1, 1) .. DateTime(2013, 1, 31)]
|> Series.mapKeys (fun k -> k.ToShortDateString())

// GROUPING DATA

let travels = people.GetColumn<string list>("Countries")

// Group by name length (ignoring visited countries)
travels |> Series.groupBy (fun k v -> k.Length)
// Group by number of visited countries 
travels |> Series.groupBy (fun k v -> v.Length)
// Group by visited countries (people visited/not visited US)
travels |> Series.groupBy (fun k v -> List.exists((=) "US") v)

// Group by name length and get number of values in each group
travels |> Series.groupInto
  (fun k v -> k.Length)
  (fun len people -> Series.countKeys people)

travels
|> Series.mapValues (Seq.countBy id >> series)
|> Frame.ofRows
|> Frame.fillMissingWith 0

// Grouping data frames

// Group using column 'Sex' of type 'string'
titanic |> Frame.groupRowsByString "Sex"

// Grouping using column converted to 'decimal'
let byDecimal : Frame<decimal *_, _> =
  titanic |> Frame.groupRowsBy "Fare"
// This is easier using member syntax
titanic.GroupRowsBy<decimal>("Fare")

// Group using calculated value - length of name
titanic |> Frame.groupRowsUsing (fun k row ->
  row.GetAs<string>("Name").Length)

// Grouping by single key

titanic |> Frame.groupRowsByString "Sex"

let bySex = titanic |> Frame.groupRowsByString "Sex"
// Returns series with two frames as values
let bySex1 = bySex |> Frame.nest
let bySex2 = bySex |> Frame.nest |> Frame.unnest

// Grouping by multiple keys

// Group by passanger class and port
let byClassAndPort = 
  titanic
  |> Frame.groupRowsByInt "Pclass"
  |> Frame.groupRowsByString "Embarked"
  |> Frame.mapRowKeys Pair.flatten3

// Get just the Age series with the same row index
let ageByClassAndPort = byClassAndPort?Age

// get average ages in each group
byClassAndPort?Age |> Stats.levelMean Pair.get1And2Of3

// Averages for all numeric columns
byClassAndPort
|> Frame.getNumericCols
|> Series.dropMissing
|> Series.mapValues (Stats.levelMean Pair.get1And2Of3)
|> Frame.ofColumns

// Count number of survivors in each group
byClassAndPort.GetColumn<bool>("Survived")
|> Series.applyLevel Pair.get1And2Of3 (Series.values >> Seq.countBy id >> series)
|> Frame.ofRows

// SUMMARISING DATA WITH PIVOT TABLE

titanic
|> Frame.pivotTable
  // returns a new row key
  (fun k r -> r.GetAs<string>("Sex"))
  // returns a new row key
  (fun k r -> r.GetAs<bool>("Survived"))
  // Specifies the aggregation of the sub-frames
  Frame.countRows

// Member syntax
let table : Frame<string,bool> =
  titanic.PivotTable("Sex", "Survived", Frame.countRows)

titanic
|> Frame.pivotTable
    (fun k r -> r.GetAs<string>("Sex"))
    (fun k r -> r.GetAs<bool>("Survived"))
    (fun frame -> frame?Age |> Stats.mean)
|> round

// Hierarchical indexing

// Get all countries in the Euro area
world.Columns.["Euro area", *]
// Get Belgium data from the Euro area group
world.Columns.["Euro area", "Belgium"] //F# 3.1 required
world.Columns.[*, "Belgium"]

// Get all countries in Euro area (F# 3.0 compatable)
world.Columns.[Lookup1Of2 "Euro area"]
// Belgium is returned twice - from both Euro and OECD
world.Columns.[Lookup2Of2 "Belgium"]

// Drop the first level of keys (and get just countries)
let euro = 
  world.Columns.["Euro area", *]
  |> Frame.mapColKeys snd

// Grouping and aggregating World bank data
let decades = euro |> Frame.groupRowsUsing (fun k _ -> sprintf "%d0s" (k / 10))

decades.Rows.["1990s", *] * 1e9

// Calculate means per decades for Slovakia
decades?``Slovak Republic`` |> Stats.levelMean fst

// calculate means per decade for all countries
decades
|> Frame.getNumericCols
|> Series.mapValues (Stats.levelMean fst)
|> Frame.ofColumns

// Calculate standard deviation per decades in USD
decades?Belgium * 1.0e9
|> Stats.levelStdDev fst

// Group countries by comparing average GDP with $500bn
let byGDP = 
  decades |> Frame.transpose |> Frame.groupRowsUsing (fun k v -> v.As<float>() |> Stats.mean > 500.0)

// HANDLING MISSING VALUES

Series.ofValues [ Double.NaN; 1.0; 3.14 ]
[ Nullable(1); Nullable(); Nullable(3)] |> Series.ofValues

// Get column with missing values
let ozone = air?Ozone

// Replace missing values with zeros (but, there are better ways to handle missing)
ozone |> Series.mapAll (fun k v -> match v with None -> Some 0.0 | v -> v)

// Fill missing values with constant
ozone |> Series.fillMissingWith 0.0

// Available values are copied in backward 
// direction to fill missing values
ozone |> Series.fillMissing Direction.Backward

// Available values are propagated forward
// (if the first value is missing, it is not filled!)
ozone |> Series.fillMissing Direction.Forward

// Fill values and drop those that could not be filled
ozone |> Series.fillMissing Direction.Forward |> Series.dropMissing

// Fill missing values using interpolation function
ozone |> Series.fillMissingUsing (fun k ->
  // Get previous and next values
  let prev = ozone.TryGet(k, Lookup.ExactOrSmaller)
  let next = ozone.TryGet(k, Lookup.ExactOrGreater)
  // Pattern match to check which values were available
  match prev, next with
  | OptionalValue.Present(p), OptionalValue.Present(n) -> (p + n) / 2.0
  | OptionalValue.Present(v), _
  | _, OptionalValue.Present(v) -> v
  | _ -> 0.0)