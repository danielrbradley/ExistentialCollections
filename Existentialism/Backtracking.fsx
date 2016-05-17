module Backtracking

type FieldName = FieldName of string

// A tree with at least one element in each branch
type NonEmptyList<'T> =
  {
    Head : 'T
    Tail : 'T list
  }

module NonEmptyList =
  let singleton item =
    {
      Head = item
      Tail = []
    }

  let append x y =
    { x with Tail = List.append x.Tail (y.Head :: y.Tail) }

  let collect lists =
    match lists with 
    | [] -> invalidArg "lists" "Cannot be an empty list"
    | head :: tail ->
      tail |> List.fold append head

  let cons head tail =
    {
      Head = head
      Tail = tail.Head :: tail.Tail
    }

  let ofList list =
    match list with 
    | [] -> invalidArg "lists" "Cannot be an empty list"
    | head :: tail ->
      { Head = head
        Tail = tail }

type Trace<'a> =
  | Root of 'a
  | Affect of 'a * NonEmptyList<Trace<'a>>

module Trace =
  let start x =
    Root x

  let addAffect affect trace =
    Affect(affect, NonEmptyList.singleton trace)

  let combine affect traceA traceB =
    Affect(affect, { Head = traceA; Tail = [traceB] })

  let collect context traces =
    Affect(context, traces)

type Awareness<'T> = 
  | Known of 'T
  | Unknown of trace:FieldName Trace

module Awareness =
  let all (context : FieldName) (source : Awareness<'a> list) : Awareness<'a list> =
    let unknownTraces =
      source
      |> List.choose (function
        | Known _ -> None
        | Unknown trace -> Some trace)
    if unknownTraces <> [] then
      unknownTraces
      |> NonEmptyList.ofList
      |> Trace.collect context
      |> Unknown
    else
      Known(source |> List.map (function
        | Known x -> x
        | _ -> invalidOp "Found unknowns after check."))

  let both context x y =
      match x, y with
      | Known xValue, Known yValue -> Known(xValue, yValue)
      | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
      | Unknown xTrace, Unknown yTrace ->
        Trace.combine context xTrace yTrace
        |> Unknown
