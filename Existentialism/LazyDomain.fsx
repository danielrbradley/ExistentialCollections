#load "Backtracking.fsx"
open Backtracking

type FieldBuilder(name : string) =
  member this.Return(x) =
    lazy (Known x)
  member this.ReturnFrom(x : Lazy<Awareness<'a>>) =
    x
  member this.Bind(m : Lazy<Awareness<'a>>, f : 'a -> Lazy<Awareness<'b>>) =
    lazy
      match m.Value with
      | Known x ->
        match (f x).Value with
        | Known x -> Known x
        | Unknown trace ->
          Unknown <| NonEmptyList.append trace (NonEmptyList.singleton (FieldName name))
      | Unknown trace->
        Unknown <| NonEmptyList.append trace (NonEmptyList.singleton (FieldName name))
  member this.Bind(m : Awareness<'a>, f : 'a -> Lazy<Awareness<'b>>) =
    lazy
      match m with
      | Known x ->
        match (f x).Value with
        | Known x -> Known x
        | Unknown trace ->
          Unknown <| NonEmptyList.append trace (NonEmptyList.singleton (FieldName name))
      | Unknown trace->
        Unknown <| NonEmptyList.append trace (NonEmptyList.singleton (FieldName name))

let field name = new FieldBuilder(name)

let foo =
  field("Foo") {
    return 1
  }

let bar =
  field("Bar") {
    let! foo = foo
    let inc = foo + 1
    return inc
  }

let bob =
  field("Bob") {
    return! bar
  }

type Instrument =
  {
    TotalSharesOutstanding : Awareness<decimal>
  }

type AssetInputFields =
  {
    Quantity : Awareness<decimal>
  }

type AssetCalculatedFields =
  {
    PercentTotalSharesOutstanding : decimal Awareness Lazy
  }

type Asset =
  {
    Instrument : Instrument
    InputFields : AssetInputFields
    CalculatedFields : AssetCalculatedFields
  }

let makeAsset instrument inputFields =
  { Instrument = instrument
    InputFields = inputFields
    CalculatedFields =
      {
        PercentTotalSharesOutstanding =
          field ("PercentTotalSharesOutstanding") {
            let! quantity, totalSharesOutstanding =
              Awareness.both (inputFields.Quantity) (instrument.TotalSharesOutstanding)
            return quantity / totalSharesOutstanding
          }
      }
  }

let asset =
  makeAsset
    { TotalSharesOutstanding = Known 42m }
    { Quantity = Unknown (NonEmptyList.singleton (FieldName "Quantity")) }

asset.CalculatedFields.PercentTotalSharesOutstanding.Value
