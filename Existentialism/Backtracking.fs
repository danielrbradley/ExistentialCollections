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

  let addIfNewFrame frame trace =
    if trace.Frame = frame then trace
    else trace |> addFrame frame

  let causesExceptSelf currentFrame trace =
    if trace.Frame = currentFrame then trace.Causes
    else [trace]

  let combine frame traces =
    {
      Frame = frame
      Causes = traces |> List.collect (causesExceptSelf frame)
    }

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

  let both (getFrame : unit -> 'f) (a : Awareness<'a, 'f>, b : Awareness<'b, 'f>) : Awareness<'a * 'b, 'f> =
    match a, b with
    | Known a', Known b' -> Known(a', b')
    | Unknown t1, Unknown t2 ->
      let frame = getFrame()
      let causes1 = t1 |> Trace.causesExceptSelf frame
      let causes2 = t2 |> Trace.causesExceptSelf frame
      Unknown ({ Frame = frame; Causes = List.append causes1 causes2 })
    | _, Unknown t | Unknown t, _ -> Unknown (Trace.addIfNewFrame(getFrame()) t)

type AwarenessBuilder<'f when 'f : equality> (getFrame : unit -> 'f) =
  member this.Zero() =
    Unknown (Trace.root (getFrame()))
  member this.Return(x) =
    Known x
  member this.ReturnFrom(x) =
    match x with
    | Known _ -> x
    | Unknown t ->
      Unknown (Trace.addFrame (getFrame()) t)
  member this.Bind(a : Awareness<'a, 'f>, f : 'a -> Awareness<'b, 'f>) =
    match a with
    | Unknown t1 ->
      Unknown (Trace.addIfNewFrame (getFrame()) t1)
    | Known x ->
      match f x with
      | Known b -> Known b
      | Unknown t ->
        Unknown <| Trace.addIfNewFrame (getFrame()) t

let awareness (getFrame) =
  new AwarenessBuilder<_>(getFrame)
