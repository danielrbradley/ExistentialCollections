﻿namespace ExistentialCollections

type Existance = 
    | Exists
    | Speculative

type Awareness<'a> = 
    | Known of 'a
    | Unknown

type ExNumber<'a> =
    | Exact of 'a
    | Estimation of excludingUnknowns:'a * min:'a * max:'a

type ExistentialList<'a> = (Existance * 'a) list

type ExistentialMap<'a, 'b when 'a : comparison> = Map<Existance * 'a, 'b>

module Option = 
    let getOrDefault defaultValue opt = 
        match opt with
        | Some v -> v
        | None -> defaultValue

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExList = 
    let ofList source : ExistentialList<'a> = source |> List.map (fun item -> Exists, item)
    
    let filter (predicate : 'a -> Awareness<bool>) (source : ExistentialList<'a>) : ExistentialList<'a> = 
        let folder (existance : Existance, item : 'a) (filtered : ExistentialList<'a>) : ExistentialList<'a> = 
            match item |> predicate with
            | Unknown -> (Speculative, item) :: filtered
            | Known true -> (existance, item) :: filtered
            | Known false -> filtered
        List.foldBack folder source []
    
    let groupBy (keySelector : 'a -> Awareness<'b>) (source : ExistentialList<'a>) : ExistentialMap<Awareness<'b>, ExistentialList<'a>> = 
        let folder (existance : Existance, item : 'a) (groups : ExistentialMap<Awareness<'b>, ExistentialList<'a>>) : ExistentialMap<Awareness<'b>, ExistentialList<'a>> = 
            let key = 
                match keySelector item with
                | Unknown -> Speculative, Unknown
                | Known k -> Exists, Known k
            
            let addedtoOwnGroup = 
                match groups |> Map.tryFind key with
                | None -> 
                    let unknownsList = 
                        groups
                        |> Map.tryFind (Speculative, Unknown)
                        |> Option.getOrDefault []
                        |> List.map (fun (_, value) -> Speculative, value)
                    groups |> Map.add key ((existance, item) :: unknownsList)
                | Some list -> groups |> Map.add key ((existance, item) :: list)
            
            match key with
            | _, Unknown -> 
                addedtoOwnGroup |> Map.map (fun key list -> 
                                       if key = (Speculative, Unknown) then list
                                       else (Speculative, item) :: list)
            | _, Known k -> addedtoOwnGroup
        List.foldBack folder source Map.empty
    
    let map (mapping : 'a -> 'b) (source : ExistentialList<'a>) : ExistentialList<'b> = 
        source |> List.map (fun (ex, item) -> ex, mapping item)

    let inline sum (source : ExistentialList<'a>) : ExNumber<'a> when ^a : (static member ( + ) :  ^a *  ^a ->  ^a) and ^a : (static member Zero :  ^a) =
        let zero = LanguagePrimitives.GenericZero< (^a) >
        let fold (existance:Existance, item:'a) (aggregate : ExNumber<'a>) : ExNumber<'a> =
            match existance with
            | Exists ->
                match aggregate with
                | Exact value -> Exact (value + item)
                | Estimation (excludingUnknowns, min, max) -> Estimation (excludingUnknowns + item, min + item, max + item)
            | Speculative when item <> zero ->
                match aggregate with
                | Exact value -> Estimation (value + item, value + (min item zero), value + (max item zero))
                | Estimation (excludingUnknowns, currentMin, currentMax) -> 
                    Estimation (excludingUnknowns + item, currentMin + (min item zero), currentMax + (max item zero))
            | _ -> aggregate
        List.foldBack fold source (Exact zero)
