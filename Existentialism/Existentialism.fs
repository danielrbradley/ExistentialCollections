﻿namespace Existentialism

// Note to self - don't try to continue on summing unknowns or speculations, 
// instead look at modelling the failure case to expain why it's not possible.

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
    static member inline DivideByInt (x : Awareness<'b>) (y : int) : Awareness<'b> =
        match x with
        | Known value -> Known <| LanguagePrimitives.DivideByInt< (^b) > value y
        | Unknown -> Unknown
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

type ExList<'a> = Existance<'a> list

// INTERNAL MONOLOGUE:
// Should exlookup really have keys which have existance?
// Makes sense for ExMap because there is only one item per existance of the key?
// But then it doesn't make sense that it could then both exist and speculatively exist.
// So, should ExMap apply the existance to the value rather than the key
// and likewise, ExLookup just have existance within the item list?
type ExMap<'a, 'b when 'a : comparison> = Map<'a, Existance<'b>>

type ExLookup<'a, 'b when 'a : comparison> = Map<'a, ExList<'b>>

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
    let inline divideByInt (x : Awareness<'a>) y =
        match x with
        | Known x' -> Known <| LanguagePrimitives.DivideByInt x' y
        | Unknown -> Unknown

    let fromOption opt =
        match opt with
        | Some value -> Known value
        | None -> Unknown

    let map (mapping : 'a -> 'b) (source : Awareness<'a>) : Awareness<'b> =
        match source with
        | Known value -> Known <| mapping value
        | Unknown -> Unknown

    let toOption (awareness : Awareness<'a>) : Option<'a> =
        match awareness with
        | Known x -> Some x
        | Unknown -> None

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExList = 
    let append (source1 : ExList<'a>) (source2 : ExList<'a>) : ExList<'a> =
        List.append source1 source2
    
    let inline average (source : ExList<'a>) : Awareness<'a> when ^a : (static member ( + ) :  ^a *  ^a ->  ^a) and ^a : (static member Zero :  ^a)  and ^a : (static member DivideByInt :  ^a * int ->  ^a) =
        if source = [] then raise (System.ArgumentException "The input list was empty")
        let zero = LanguagePrimitives.GenericZero< (^a) >
        let rec averageInternal (remaining : ExList<'a>, aggregate : 'a, count : int) : Awareness<'a> =
            match remaining with
            | [] -> Known (LanguagePrimitives.DivideByInt aggregate count)
            | head :: tail ->
                match head with
                | Speculative _ -> Unknown
                | Exists value ->
                    averageInternal(tail, value + aggregate, count + 1)
        averageInternal (source, zero, 0)

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

    let filter (predicate : 'a -> bool) (list : ExList<'a>) : ExList<'a> =
        List.filter (Existance.value >> predicate) list

    let filterAwareness (predicate : 'a -> Awareness<bool>) (source : ExList<'a>) : ExList<'a> = 
        let folder (item : Existance<'a>) (filtered : ExList<'a>) : ExList<'a> =
            let value = item.Value
            match value |> predicate with
                | Known true -> item :: filtered
                | Known false -> filtered
                | Unknown -> Speculative value :: filtered
        List.foldBack folder source []

    let filterByAwareness (projection : 'a -> Awareness<'b>) (predicate : 'b -> bool) (source : ExList<'a>) : ExList<'a> = 
        source |> filterAwareness (projection >> (Awareness.map predicate))

    let forall (predicate : 'a -> bool) (source : ExList<'a>) : Awareness<bool> =
        let rec forall' (state : Awareness<bool>) (remaining : ExList<'a>) : Awareness<bool> =
            match remaining with
            | [] -> state
            | head :: tail ->
                match head with
                | Exists value -> 
                    match predicate value with
                    | true -> forall' state tail
                    | false -> Known false
                | Speculative value ->
                    match predicate value with
                    | true -> forall' state tail
                    | false -> forall' Unknown tail
        forall' (Known true) source

    let forallAwareness (predicate : 'a -> Awareness<bool>) (source : ExList<'a>) : Awareness<bool> =
        let rec checkNext (state : Awareness<bool>) (remaining : ExList<'a>) : Awareness<bool> =
            match remaining with 
            | [] -> state
            | head :: tail ->
                match head with
                | Exists value -> 
                    match predicate value with
                    | Known true -> checkNext state tail
                    | Known false -> Known false
                    | Unknown -> Unknown
                | Speculative value ->
                    match predicate value with
                    | Known true -> checkNext state tail
                    | Known false -> checkNext Unknown tail
                    | Unknown -> Unknown
        checkNext (Known true) source

    let groupBy (keySelector : 'T -> 'TKey) (source : ExList<'T>) : ExLookup<'TKey, 'T> =
        let folder (item : Existance<'T>) (aggregate : ExLookup<'TKey, 'T>) : ExLookup<'TKey, 'T> =
            let key = keySelector (item.Value)
            let groupOpt = aggregate |> Map.tryFind key
            match groupOpt with
            | None -> // Add new group
                aggregate.Add(key, [item])
            | Some group ->
                aggregate.Remove(key).Add(key, item :: group)
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

    let inline sum (source : ExList<'a>) : Awareness<'a> when ^a : (static member ( + ) :  ^a *  ^a ->  ^a) and ^a : (static member Zero :  ^a) =
        let zero = LanguagePrimitives.GenericZero< (^a) >
        let rec sumInternal (remaining : ExList<'a>) (aggregate : 'a) : Awareness<'a> =
            match remaining with
            | [] -> Known aggregate
            | head :: tail ->
                match head with
                | Speculative _ -> Unknown
                | Exists value ->
                    sumInternal tail (aggregate + value)
        sumInternal source zero

    let inline sumBy (mapping : 'a -> 'b) (source : ExList<'a>) : Awareness<'b> when ^b : (static member ( + ) :  ^b *  ^b ->  ^b) and ^b : (static member Zero :  ^b) =
        source |> map mapping |> sum

    let tail (list : ExList<'a>) = list.Tail

    let values (source : ExList<'a>) : 'a list =
        source |> List.map (fun item -> item.Value)

module ExMap =
    let map (projection : 'k -> 'a -> 'b) (source : ExMap<'k, 'a>) : ExMap<'k, 'b> =
        source |> Map.map (fun key -> Existance.map (fun v -> projection key v))

module ExLookup =
    let map (projection : 'k -> 'a -> 'b) (source : ExLookup<'k, 'a>) : ExLookup<'k, 'b> =
        source |> Map.map (fun key list -> list |> ExList.map (projection key))

    let inline sum (source : ExLookup<'a, 'b>) : Map<'a, Awareness<'b>> when ^b : (static member ( + ) :  ^b *  ^b ->  ^b) and ^b : (static member Zero :  ^b) =
        source
        |> Map.map (fun _ exList -> exList |> ExList.sum)

    let inline sumBy (projection : 'a -> 'b) (source : ExLookup<'key,'a>) : Map<'key, Awareness<'b>> when ^b : (static member ( + ) :  ^b *  ^b ->  ^b) and ^b : (static member Zero :  ^b) =
        source
        |> Map.map (fun _ -> ExList.sumBy projection)
