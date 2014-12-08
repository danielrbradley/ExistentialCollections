#load "Existentialism.fs"

open ExistentialCollections
open System

// This represents a specific "position" aka a number of shares we hold in a company.
type Holding = 
    { Name : string
      Quantity : decimal
      // We might or might not know the current share price or where the shares are from.
      Jurisdiction : Awareness<string>
      SharePrice : Awareness<decimal> }

// Here's the shares we own:
let portfolio = 
    [ { Name = "Microsoft"
        Quantity = 500M
        Jurisdiction = Known "US"
        SharePrice = Known 48.89M }
      { Name = "Apple"
        Quantity = 650M
        Jurisdiction = Known "US"
        SharePrice = Known 108.83M }
      { Name = "Comcast"
        Quantity = 300M
        Jurisdiction = Known "US"
        SharePrice = Unknown }
      { Name = "BT Group"
        Quantity = 200M
        Jurisdiction = Known "UK"
        SharePrice = Known 375.10M }
      { Name = "IBM"
        Quantity = 250M
        Jurisdiction = Unknown
        SharePrice = Known 164.07M } ]

(* 1:
    We can make any regular list into an 'existential' list. 
    This just takes each item and says we know it exists. *)
let exPortfolio = portfolio |> ExList.ofList
(* 2:
    Next, we need to group the shares by which country they came from.
    Where we don't know the jurisdiction, it mean that it could potentially be in any of the groups, 
    or it could be in another unknown group. *)
let portfolioByJurisdiction = exPortfolio |> ExList.groupBy (fun holding -> holding.Jurisdiction)
(* 3:
    Now, we can calculate how much money we have in each company by multiplying the share price by the 
    number of shares we own. *)
let holdingValueByJurisidiction = 
    portfolioByJurisdiction |> ExLookup.map (fun _ holding -> holding.SharePrice * (Known holding.Quantity))
(* 4:
    Finally, we can calculate how much money we have invested in each country.
    What's interesting in this result, is that it tells us all determinable information possible 
    given the missing information. *)
let totalValueByJurisdiction = holdingValueByJurisidiction |> ExLookup.sum

// Or, all in one line ...
let moneyInEachCountry = 
    exPortfolio
    |> ExList.groupBy (fun holding -> holding.Jurisdiction)
    |> ExLookup.sumBy (fun holding -> holding.SharePrice * (Known holding.Quantity))
(* Output:
    [(Exists (Known "UK"), Estimation {ExcludingUnknowns = Known 150040.00M;
                                       Minimum = Known 75020.00M;
                                       Maximum = Known 150040.00M;});
     (Exists (Known "US"), Estimation {ExcludingUnknowns = Unknown;
                                       Minimum = Unknown;
                                       Maximum = Unknown;});
     (Speculative Unknown, Exact (Known 41017.50M))] *)
