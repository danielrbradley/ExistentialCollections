module Backtracking

module List =
  let partitionMap (mapping : 'a -> Choice<'b, 'c>) (source : 'a list) : 'b list * 'c list =
    List.foldBack
      (fun next (bList, cList) ->
        match mapping next with
        | Choice1Of2 b -> (b :: bList), cList
        | Choice2Of2 c -> bList, (c :: cList))
      source
      ([],[])

// A tree with at least one element in each branch
type Trace<'TFrame> =
  {
    Frame : 'TFrame
    Causes : Trace<'TFrame> list
  }

module Trace =
  let root frame =
    { Frame = frame; Causes = [] }

  let addFrame frame trace =
    { Frame = frame; Causes = [trace] }

  let combine frame traces =
    { Frame = frame; Causes = traces }

type Awareness<'T, 'TFrame> = 
  | Known of 'T
  | Unknown of 'TFrame Trace

module Awareness =
  let collect (aggregate : 'a list -> 'b) (getFrame : unit -> 'f) (source : Awareness<'a,'f> list) : Awareness<'b, 'f> =
    let knowns, unknowns =
      source
      |> List.partitionMap (function
        | Known a -> Choice1Of2 a
        | Unknown b -> Choice2Of2 b)
    match unknowns with
    | [] -> Known (aggregate knowns)
    | _ ->
      let trace = unknowns |> Trace.combine (getFrame())
      Unknown trace

type AwarenessBuilder<'f> (getFrame : unit -> 'f) =
  member this.Return(x) =
    Known x
  member this.ReturnFrom(x) =
    match x with
    | Known _ -> x
    | Unknown t ->
      Unknown (Trace.addFrame (getFrame()) t)
  member this.Bind(a, f) =
    match a with
    | Unknown t ->
      Unknown (Trace.addFrame (getFrame()) t)
    | Known x ->
      f x

let awareness (getFrame) =
  new AwarenessBuilder<_>(getFrame)
