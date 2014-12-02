namespace ExistentialismTests

open Swensen.Unquote
open Microsoft.VisualStudio.TestTools.UnitTesting

open ExistentialCollections

[<TestClass>]
type ``Awareness specifications``() =
    [<TestMethod>]
    member x.``Adding knowns gives known`` () =
        let total = (Known 4M) + (Known 2M)
        test <@ total = Known 6M @>

    [<TestMethod>]
    member x.``Adding known with unknown gives unknown`` () =
        let total = (Known 4M) + Unknown
        test <@ total = Unknown @>

    [<TestMethod>]
    member x.``Is divisible by an int`` () =
        test <@ Awareness.divideByInt (Known 4M) 2 = Known 2M @>

[<TestClass>]
type ``ExList averaging specifications``() =
    [<TestMethod>]
    member x.``Can't average empty list`` () =
        raises <@ List.empty<Existance<decimal>> |> ExList.average @>

    [<TestMethod>]
    member x.``Average of single known is itself`` () =
        test <@ [Exists 42M] |> ExList.average = Known 42M @>

    [<TestMethod>]
    member x.``Averages all-knowns to single known``() =
        test <@ [Exists 2M; Exists 4M] |> ExList.average = Known 3M @>

    [<TestMethod>]
    member x.``Any speculative entries results in unknown``() =
        test <@ [Exists 2M; Speculative 4M] |> ExList.average = Unknown @>
