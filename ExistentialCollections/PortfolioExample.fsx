#load "Existentialism.fs"

open ExistentialCollections
open System

type Holding = 
    { Name : string
      Jurisdiction : string Awareness
      SharePrice : decimal Awareness
      Quantity : int }

let portfolio = 
    [ { Name = "Microsoft"
        Jurisdiction = Known "US"
        SharePrice = Known 48.89M
        Quantity = 500 }
      { Name = "Apple"
        Jurisdiction = Known "US"
        SharePrice = Known 108.83M
        Quantity = 650 }
      { Name = "Comcast"
        Jurisdiction = Known "US"
        SharePrice = Unknown
        Quantity = 300 }
      { Name = "BT Group"
        Jurisdiction = Known "UK"
        SharePrice = Known 375.10M
        Quantity = 200 }
      { Name = "IBM"
        Jurisdiction = Unknown
        SharePrice = Known 164.07M
        Quantity = 250 } ]

let exPortfolio = portfolio |> ExList.ofList
let portfolioByJurisdiction = exPortfolio |> ExList.groupBy (fun holding -> holding.Jurisdiction)
let holdingValueByJurisidiction = 
    portfolioByJurisdiction 
    |> ExLookup.map 
           (fun _ holding -> holding.SharePrice |> Awareness.map (fun price -> price * decimal holding.Quantity))
let totalValueByJurisdiction = holdingValueByJurisidiction |> ExLookup.sum
