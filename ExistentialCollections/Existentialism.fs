namespace ExistentialCollections

type Existance = 
    | Exists
    | Speculative

type Awareness<'a> = 
    | Known of 'a
    | Unknown

type Estimation<'a> =
    { ExcludingUnknowns : 'a
      Minimum : 'a
      Maximum : 'a }

type ExNumber<'a> =
    | Exact of 'a
    | Estimation of Estimation<'a>

type ExList<'a> = (Existance * 'a) list

type ExMap<'a, 'b when 'a : comparison> = Map<Existance * 'a, 'b>

module private Option = 
    let getOrDefault defaultValue opt = 
        match opt with
        | Some v -> v
        | None -> defaultValue

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExList = 
    let ofList source : ExList<'a> = source |> List.map (fun item -> Exists, item)
    
    let filter (predicate : 'a -> Awareness<bool>) (source : ExList<'a>) : ExList<'a> = 
        let folder (existance : Existance, item : 'a) (filtered : ExList<'a>) : ExList<'a> = 
            match item |> predicate with
            | Unknown -> (Speculative, item) :: filtered
            | Known true -> (existance, item) :: filtered
            | Known false -> filtered
        List.foldBack folder source []
    
    let groupBy (keySelector : 'a -> Awareness<'b>) (source : ExList<'a>) : ExMap<Awareness<'b>, ExList<'a>> = 
        let folder (existance : Existance, item : 'a) (groups : ExMap<Awareness<'b>, ExList<'a>>) : ExMap<Awareness<'b>, ExList<'a>> = 
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
    
    let map (mapping : 'a -> 'b) (source : ExList<'a>) : ExList<'b> = 
        source |> List.map (fun (ex, item) -> ex, mapping item)

    let inline sum (source : ExList<'a>) : ExNumber<'a> when ^a : (static member ( + ) :  ^a *  ^a ->  ^a) and ^a : (static member Zero :  ^a) =
        let zero = LanguagePrimitives.GenericZero< (^a) >
        let fold (existance:Existance, item:'a) (aggregate : ExNumber<'a>) : ExNumber<'a> =
            match existance with
            | Exists ->
                match aggregate with
                | Exact value -> Exact (value + item)
                | Estimation (estimation) -> 
                    Estimation { ExcludingUnknowns = estimation.ExcludingUnknowns + item
                                 Minimum = estimation.Minimum + item
                                 Maximum = estimation.Maximum + item }
            | Speculative when item <> zero ->
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
