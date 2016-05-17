module Tests.``Backtracking Awareness``

open Backtracking
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Can return value`` () =
  test
    <@
      awareness(fun _ -> "context") {
        return 1
      } = Known 1
    @>

let unknownRoot : Awareness<int, string> = Unknown (Trace.root "Root")
let unknownWithContext : Awareness<int, string> = Unknown ({ Frame =  "context"; Causes = [ { Frame = "Root"; Causes = [] } ] })

[<Fact>]
let ``Returning known adds context`` () =
  test
    <@
      awareness(fun _ -> "context") {
        return! unknownRoot
      } = unknownWithContext
    @>

[<Fact>]
let ``Binding from an unknown adds context``() =
  test
    <@
      awareness(fun _ -> "context") {
        let! x = unknownRoot
        return x
      } = unknownWithContext
    @>

[<Fact>]
let ``Can bind from known``() =
  test
    <@
      awareness(fun _ -> "context") {
        let! x = Known 1
        return x
      } = Known 1
    @>

[<Fact>]
let ``Can bind from unknown``() =
  test
    <@
      awareness(fun _ -> "context") {
        let! x = unknownRoot
        return x
      } = unknownWithContext
    @>

[<Fact>]
let ``Can bind from unknown 2``() =
  test
    <@
      awareness(fun _ -> "context") {
        let! x = unknownRoot
        let! y = Known 1
        return x + y
      } = unknownWithContext
    @>

let unknownWithDoubleContext : Awareness<int, string> =
  Unknown (
    { Frame =  "context";
      Causes =
        [ { Frame =  "context"
            Causes =
              [ { Frame = "Root"
                  Causes = [] } ] } ] })

[<Fact>]
let ``Can bind from unknown 3``() =
  test
    <@
      awareness(fun _ -> "context") {
        let! y = Known 1
        let! x = unknownRoot
        return x + y
      } = unknownWithContext
    @>

[<Fact>]
let ``Can bind from 2 knowns``() =
  test
    <@
      awareness(fun _ -> "context") {
        let! y = Known 1
        let! x = Known 2
        return x + y
      } = Known 3
    @>

[<Fact>]
let ``Can bind from 2 unknowns``() =
  test
    <@
      awareness(fun _ -> "context") {
        let! y = unknownRoot
        let! x = unknownRoot
        return x + y
      } = unknownWithContext
    @>
