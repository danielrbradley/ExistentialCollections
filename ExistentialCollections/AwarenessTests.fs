module Tests.``Awareness Specifications``

open Existentialism
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Adding knowns gives known`` () =
    let total = (Known 4M) + (Known 2M)
    test <@ total = Known 6M @>

[<Fact>]
let ``Adding known with unknown gives unknown`` () =
    let total = (Known 4M) + Unknown
    test <@ total = Unknown @>

[<Fact>]
let ``Known is divisible by an int`` () =
    test <@ Awareness.divideByInt (Known 4M) 2 = Known 2M @>

[<Fact>]
let ``Unknown is divisible by an int`` () =
    test <@ Awareness.divideByInt (Awareness<decimal>.Unknown) 2 = Unknown @>
