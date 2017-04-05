(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**
Chapter 1
================
First, we can access the world bank data provider
*)
open FSharp.Data
let wb = WorldBankData.GetDataContext()

(**
Now we can query the data provider to see some interesting stuff
*)

wb.Countries.``United Kingdom``.Indicators.``GDP (current US$)``

(**
Chapter 2
================
Using F# And deedle
*)

#r "System.Xml.Linq.dll"

open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle

type WorldData = XmlProvider<"http://api.worldbank.org/countries/indicators/NY.GDP.PCAP.CD?date=2010:2010">

let indUrl = "http://api.worldbank.org/countries/indicators"

let getData year indicator = 
    let query = [("per_page","1000");("date",sprintf "%d:%d" year year)]
    let data = Http.RequestString(indUrl + indicator, query)
    let xml = WorldData.Parse(data)
    let orNaN value = defaultArg (Option.map float value) nan
    series [for d in xml.Datas -> d.Country.Value, orNaN d.Value]

(**
world bank data as loaded in chapter 1
*)

let inds = wb.Countries.World.Indicators
let code = inds.``GDP (current US$)``.IndicatorCode

let co2000 = getData 2000 code
let co2010 = getData 2010 code
let co2015 = getData 2015 code

let change = (co2010 - co2000) / co2000 * 100.0