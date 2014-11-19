namespace ExistentialCollections

type Existance<'a> = 
    | Exists of 'a
    | Speculative of 'a
    member ex.Value = 
        match ex with
        | Exists value | Speculative value -> value

type Awareness<'a> = 
    | Known of 'a
    | Unknown
    static member inline get_Zero () : Awareness<'b> =
        Known (LanguagePrimitives.GenericZero< (^b) >)
    static member inline (+) (x, y) = 
        match x, y with
        | Known xValue, Known yValue -> Known(xValue + yValue)
        | _, _ -> Unknown
    static member inline (-) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue - yValue)
        | _, _ -> Unknown
    static member inline (*) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue * yValue)
        | _, _ -> Unknown
    static member inline (/) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue / yValue)
        | _, _ -> Unknown
    static member inline (%) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue % yValue)
        | _, _ -> Unknown
    static member inline (&&&) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue &&& yValue)
        | _, _ -> Unknown
    static member inline (|||) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue ||| yValue)
        | _, _ -> Unknown
    static member inline (^^^) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue ^^^ yValue)
        | _, _ -> Unknown
    static member inline (<<<) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue <<< yValue)
        | _, _ -> Unknown
    static member inline (~~~) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue ~~~ yValue)
        | _, _ -> Unknown
    static member inline (>>>) (x, y) =
        match x, y with
        | Known xValue, Known yValue -> Known(xValue >>> yValue)
        | _, _ -> Unknown

type Estimation<'a> =
    { ExcludingUnknowns : 'a
      Minimum : 'a
      Maximum : 'a }

type ExNumber<'a> =
    | Exact of 'a
    | Estimation of Estimation<'a>

type ExList<'a> = Existance<'a> list

type ExMap<'a, 'b when 'a : comparison> = Map<Existance<'a>, 'b>

type ExLookup<'a, 'b when 'a : comparison> = ExMap<'a, ExList<'b>>

module private Option = 
    let getOrDefault defaultValue opt = 
        match opt with
        | Some v -> v
        | None -> defaultValue

module Existance =
    let map (mapping : 'a -> 'b) (source : Existance<'a>) : Existance<'b> =
        match source with
        | Exists x -> Exists (mapping x)
        | Speculative x -> Speculative (mapping x)

    let value (item : Existance<'a>) : 'a =
        item.Value

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Awareness =
    let fromOption opt =
        match opt with
        | Some value -> Known value
        | None -> Unknown

    let map (mapping : 'a -> 'b) (source : Awareness<'a>) : Awareness<'b> =
        match source with
        | Known value -> Known <| mapping value
        | Unknown -> Unknown

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExList = 
    let append (source1 : ExList<'a>) (source2 : ExList<'a>) : ExList<'a> =
        List.append source1 source2
    
    let choose (chooser : 'a -> 'b option) (source : ExList<'a>) : ExList<'b> =
        let fold (item : Existance<'a>) target =
            match item with
            | Exists value ->
                match chooser value with
                | None -> target
                | Some chosen -> Exists chosen :: target
            | Speculative value ->
                match chooser value with
                | None -> target
                | Some chosen -> Speculative chosen :: target
        List.foldBack fold source []

    let collect (mapping : 'a -> 'b list) (source : ExList<'a>) : ExList<'b> =
        let fold (item : Existance<'a>) target =
            match item with
            | Exists value -> List.append (value |> mapping |> List.map Exists) target
            | Speculative value -> List.append (value |> mapping |> List.map Speculative) target
        List.foldBack fold source []

    let concat (lists : ExList<'a> seq) : ExList<'a> = List.concat lists
    let empty : ExList<'a> = []
    
    let exists (predicate : 'a -> bool) (source : ExList<'a>) : Awareness<bool> =
        let rec exists' (state : Awareness<bool>) (remaining : ExList<'a>) : Awareness<bool> =
            match remaining with
            | head :: tail ->
                match head, predicate head.Value with
                | _, false -> exists' state tail
                | Exists _, true -> Known true
                | Speculative _, true -> exists' Unknown tail
            | [] -> state
        exists' (Known false) source

    let existsAwareness (predicate : 'a -> Awareness<bool>) (source : ExList<'a>) : Awareness<bool> =
        let rec exists' (state : Awareness<bool>) (remaining : ExList<'a>) : Awareness<bool> =
            match remaining with
            | head :: tail ->
                match head, predicate head.Value with
                | _, Unknown -> exists' Unknown tail
                | _, Known false -> exists' state tail
                | Exists _, Known true -> Known true
                | Speculative _, Known true -> exists' Unknown tail
            | [] -> state
        exists' (Known false) source

    let filter (predicate : 'a -> Awareness<bool>) (source : ExList<'a>) : ExList<'a> = 
        let folder (item : Existance<'a>) (filtered : ExList<'a>) : ExList<'a> =
            let value = item.Value
            match value |> predicate with
                | Known true -> item :: filtered
                | Known false -> filtered
                | Unknown -> Speculative value :: filtered
        List.foldBack folder source []
    
    let forall (predicate : 'a -> bool) (source : ExList<'a>) : Existance<bool> =
        let rec forall' (state : Existance<bool>) (remaining : ExList<'a>) : Existance<bool> =
            match remaining with
            | [] -> state
            | head :: tail ->
                match head with
                | Exists value -> 
                    match predicate value with
                    | true -> forall' state tail
                    | false -> Exists false
                | Speculative value ->
                    match predicate value with
                    | true -> forall' state tail
                    | false -> forall' (Speculative false) tail
        forall' (Exists true) source

    let groupBy (keySelector : 'a -> Awareness<'b>) (source : ExList<'a>) : ExLookup<Awareness<'b>, 'a> = 
        let folder (item : Existance<'a>) (groups : ExLookup<Awareness<'b>, 'a>) : ExLookup<Awareness<'b>, 'a> = 
            let itemValue = item.Value
            let key = 
                match keySelector itemValue with
                | Unknown -> Speculative  Unknown
                | Known k -> Exists (Known k)
            
            let addedtoOwnGroup = 
                match groups |> Map.tryFind key with
                | None -> 
                    let unknownsList = 
                        groups
                        |> Map.tryFind (Speculative Unknown)
                        |> Option.getOrDefault []
                        |> List.map (fun item -> Speculative (itemValue))
                    groups |> Map.add key (item :: unknownsList)
                | Some list -> groups |> Map.add key (item :: list)
            
            match key.Value with
            | Unknown -> 
                addedtoOwnGroup |> Map.map (fun key list -> 
                                       if key = (Speculative Unknown) then list
                                       else (Speculative itemValue) :: list)
            | Known k -> addedtoOwnGroup
        List.foldBack folder source Map.empty
    
    let head (source : ExList<'a>) : Existance<'a> = source.Head

    let init (length : int) (initialiser : int -> 'a) : ExList<'a> =
        List.init length (initialiser >> Exists)

    let isEmpty (source : ExList<'a>) : bool = source.IsEmpty
    let iter (action : Existance<'a> -> unit) (list : ExList<'a>) : unit = List.iter action list
    let length (source : ExList<'a>) : int = source.Length

    let map (mapping : 'a -> 'b) (source : ExList<'a>) : ExList<'b> = 
        source |> List.map (Existance.map mapping)

    let ofList source : ExList<'a> = source |> List.map (fun item -> Exists item)
    let ofArray (source : 'a []) : ExList<'a> = source |> List.ofArray |> ofList
    let ofSeq (source : 'a seq) : ExList<'a> = source |> List.ofSeq |> ofList
    let rev (source : ExList<'a>) : ExList<'a> = List.rev source

    let inline sum (source : ExList<'a>) : ExNumber<'a> when ^a : (static member ( + ) :  ^a *  ^a ->  ^a) and ^a : (static member Zero :  ^a) =
        let zero = LanguagePrimitives.GenericZero< (^a) >

        let fold (existance : Existance<'a>) (aggregate : ExNumber<'a>) : ExNumber<'a> =
            match existance with
            | Exists item ->
                match aggregate with
                | Exact value -> Exact (value + item)
                | Estimation (estimation) -> 
                    Estimation { ExcludingUnknowns = estimation.ExcludingUnknowns + item
                                 Minimum = estimation.Minimum + item
                                 Maximum = estimation.Maximum + item }
            | Speculative item when item <> zero ->
                match aggregate with
                | Exact value -> 
                    Estimation { ExcludingUnknowns = value + item
                                 Minimum = value + (min item zero)
                                 Maximum = value + (max item zero) }
                | Estimation estimation -> 
                    Estimation { ExcludingUnknowns = estimation.ExcludingUnknowns + item
                                 Minimum = estimation.Minimum + (min item zero)
                                 Maximum = estimation.Maximum + (max item zero) }
            | _ -> aggregate
        List.foldBack fold source (Exact zero)

    let inline sumBy (mapping : 'a -> 'b) (source : ExList<'a>) : ExNumber<'b> when ^b : (static member ( + ) :  ^b *  ^b ->  ^b) and ^b : (static member Zero :  ^b) =
        source |> map mapping |> sum

    let tail (list : ExList<'a>) = list.Tail

    let values (source : ExList<'a>) : 'a list =
        source |> List.map (fun item -> item.Value)

module ExMap =
    let map (projection : 'k -> 'a -> 'b) (source : ExMap<'k, 'a>) : ExMap<'k, 'b> =
        source |> Map.map (fun key -> projection key.Value)

module ExLookup =
    let map (projection : 'k -> 'a -> 'b) (source : ExLookup<'k, 'a>) : ExLookup<'k, 'b> =
        source |> Map.map (fun key list -> list |> ExList.map (projection key.Value))

    let inline sum (source : ExLookup<'a, 'b>) : ExMap<'a, ExNumber<'b>> when ^b : (static member ( + ) :  ^b *  ^b ->  ^b) and ^b : (static member Zero :  ^b) =
        source
        |> Map.map (fun _ exList -> exList |> ExList.sum)

    let inline sumBy (projection : 'a -> 'b) (source : ExLookup<'key,'a>) : ExMap<'key, ExNumber<'b>> when ^b : (static member ( + ) :  ^b *  ^b ->  ^b) and ^b : (static member Zero :  ^b) =
        source
        |> ExMap.map (fun _ -> ExList.sumBy projection)
