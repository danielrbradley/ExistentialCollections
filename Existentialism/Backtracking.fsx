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

type Awareness<'a> = 
  | Known of 'a
  | Unknown of trace:FieldName NonEmptyList
    static member inline get_Zero () : Awareness<'b> =
        Known (LanguagePrimitives.GenericZero< (^b) >)
    static member inline DivideByInt (x : Awareness<'b>) (y : int) : Awareness<'b> =
        match x with
        | Known value -> Known <| LanguagePrimitives.DivideByInt< (^b) > value y
        | Unknown trace -> Unknown trace
    static member inline (+) (x, y) = 
        match x, y with
        | Known xValue, Known yValue -> Known(xValue + yValue)
        | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
        | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)
    static member inline (-) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue - yValue)
        | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
        | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)
    static member inline (*) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue * yValue)
        | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
        | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)
    static member inline (/) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue / yValue)
        | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
        | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)
    static member inline (%) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue % yValue)
        | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
        | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)
    static member inline (&&&) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue &&& yValue)
        | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
        | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)
    static member inline (|||) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue ||| yValue)
        | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
        | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)
    static member inline (^^^) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue ^^^ yValue)
        | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
        | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)
    static member inline (<<<) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue <<< yValue)
        | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
        | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)
    static member inline (~~~) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue ~~~ yValue)
        | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
        | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)
    static member inline (>>>) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue >>> yValue)
        | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
        | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)

module Awareness =
  let all (source : Awareness<'a> list) : Awareness<'a list> =
    let unknownTraces =
      source
      |> List.choose (function
        | Known _ -> None
        | Unknown trace -> Some trace)
    if unknownTraces <> [] then
      Unknown(NonEmptyList.collect unknownTraces)
    else
      Known(source |> List.map (function
        | Known x -> x
        | _ -> invalidOp "Found unknowns after check."))

  let both x y =
      match x, y with
      | Known xValue, Known yValue -> Known(xValue, yValue)
      | Unknown trace, Known _ | Known _, Unknown trace -> Unknown trace
      | Unknown xTrace, Unknown yTrace -> Unknown (NonEmptyList.append xTrace yTrace)
