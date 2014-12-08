namespace Tests.``ExList Specifications``

open Existentialism
open Swensen.Unquote
open Xunit

module ``Averaging Specifications`` =
    [<Fact>]
    let ``Can't average empty list`` () =
        raises <@ List.empty<Existance<decimal>> |> ExList.average @>

    [<Fact>]
    let ``Average of single known is itself`` () =
        test <@ [Exists 42M] |> ExList.average = Known 42M @>

    [<Fact>]
    let ``Averages all-knowns to single known Specifications``() =
        test <@ [Exists 2M; Exists 4M] |> ExList.average = Known 3M @>

    [<Fact>]
    let ``Any speculative entries results in unknown Specifications``() =
        test <@ [Exists 2M; Speculative 4M] |> ExList.average = Unknown @>

module ``Append Specifications`` =
    [<Fact>]
    let ``Can append empty lists Specifications``() =
        test <@ ExList.append [] [] = [] @>

    [<Fact>]
    let ``Can append normal lists Specifications``() =
        test <@ ExList.append [Exists 1; Speculative 2;] [Speculative 3; Exists 4] = [Exists 1; Speculative 2; Speculative 3; Exists 4] @>

module ``Choose Specifications`` =
    [<Fact>]
    let ``Can choose from empty list Specifications``() =
        test <@ [] |> ExList.choose (fun x -> Some x) = [] @>

    [<Fact>]
    let ``Choosing maintains existance Specifications``() =
        test <@ [Exists 1; Exists 2; Speculative 3; Speculative 4] |> ExList.choose (fun x -> if x % 2 = 0 then Some x else None) = [Exists 2; Speculative 4] @>

    [<Fact>]
    let ``Can choose Knowns Specifications``() =
        test <@ [Exists (Known 1); Exists Unknown] |> ExList.choose (fun x -> x |> Awareness.toOption) = [Exists 1] @>

module ``Collect Specifications`` =
    [<Fact>]
    let ``Can collect from empty list Specifications``() =
        test <@ [] |> ExList.collect (fun x -> x) = [] @>

    [<Fact>]
    let ``Maintains existance Specifications``() =
        test <@ [Exists 2; Speculative 3] |> ExList.collect (fun x -> List.init x (fun i -> i)) = [Exists 0; Exists 1; Speculative 0; Speculative 1; Speculative 2] @>

module ``Concat Specifications`` =
    [<Fact>]
    let ``Can concat empty list Specifications``() =
        test <@ [] |> ExList.concat = ExList.empty @>

    [<Fact>]
    let ``Can concat simple list Specifications``() =
        test <@ [[Exists 1; Speculative 2]; [Speculative 3]] |> ExList.concat = [Exists 1; Speculative 2; Speculative 3] @>

module ``Exists Specifications`` =
    [<Fact>]
    let ``We known nothing exists empty list Specifications``() =
        test <@ ExList.empty |> ExList.exists (fun _ -> true) = Known false @>

    [<Fact>]
    let ``We known something exists on exists match Specifications``() =
        test <@ [Exists 1] |> ExList.exists (fun x -> x = 1) = Known true @>

    [<Fact>]
    let ``We known something doesn't exist on exists non match Specifications``() =
        test <@ [Exists 2] |> ExList.exists (fun x -> x = 1) = Known false @>

    [<Fact>]
    let ``We don't know if something exists on speculative match only Specifications``() =
        test <@ [Speculative 1] |> ExList.exists (fun x -> x = 1) = Unknown @>

    [<Fact>]
    let ``We know something exists on speculative and exists matches Specifications``() =
        test <@ [Speculative 2; Exists 4] |> ExList.exists (fun x -> x % 2 = 0) = Known true @>

module ``Exists Awareness Specifications`` =
    [<Fact>]
    let ``We known nothing exists empty list Specifications``() =
        test <@ ExList.empty |> ExList.existsAwareness (fun _ -> Known true) = Known false @>

    [<Fact>]
    let ``We known something exists on exists match Specifications``() =
        test <@ [Exists (Known 1)] |> ExList.existsAwareness (fun x ->  x |> Awareness.map (fun y -> y = 1)) = Known true @>

    [<Fact>]
    let ``We known something doesn't exist on exists non match Specifications``() =
        test <@ [Exists (Known 2)] |> ExList.existsAwareness (fun x -> x |> Awareness.map (fun y -> y = 1)) = Known false @>

    [<Fact>]
    let ``We don't know if something exists on speculative match only Specifications``() =
        test <@ [Speculative (Known 1)] |> ExList.existsAwareness (fun x -> x |> Awareness.map (fun y -> y = 1)) = Unknown @>

    [<Fact>]
    let ``We know something exists on speculative and exists known matches Specifications``() =
        test <@ [Speculative (Known 2); Exists (Known 4)] |> ExList.existsAwareness (fun x -> x |> Awareness.map (fun y -> y % 2 = 0)) = Known true @>

    [<Fact>]
    let ``We know something exists on exists known and unknown matches Specifications``() =
        test <@ [Exists (Unknown); Exists (Known 1)] |> ExList.existsAwareness (fun x -> x |> Awareness.map (fun y -> y = 1)) = Known true @>

module ``Filter Specifications`` =
    [<Fact>]
    let ``Can filter empty list``() =
        test <@ ExList.empty |> ExList.filter (fun _ -> false) = ExList.empty @>

    [<Fact>]
    let ``Maintains existance``() =
        test <@ [Exists 1; Exists 2; Speculative 3; Speculative 4] |> ExList.filter (fun x -> x % 2 = 0) = [Exists 2; Speculative 4] @>

module ``Filter Awareness Specifications`` =
    [<Fact>]
    let ``Can filter empty list``() =
        test <@ ExList.empty |> ExList.filterAwareness (fun _ -> Unknown) = ExList.empty @>

    [<Fact>]
    let ``Maintains existance for known``() =
        test <@ [Exists (Known 1); Exists (Known 2); Speculative (Known 4)] |> ExList.filterAwareness (fun x -> x |> Awareness.map (fun y -> y % 2 = 0)) = [Exists (Known 2); Speculative (Known 4)] @>

    [<Fact>]
    let ``Unknown becomes speculative``() =
        test <@ [Exists Unknown] |> ExList.filterAwareness (fun x -> x |> Awareness.map (fun y -> y % 2 = 0)) = [Speculative Unknown] @>

module ``Filter By Awareness Specification`` =
    [<Fact>]
    let ``Can filter on awareness property``() =
        test <@ [Exists (Known 1); Exists (Known 2); Speculative (Known 4)] |> ExList.filterByAwareness (fun x -> x) (fun x -> x % 2 = 0) = [Exists (Known 2); Speculative (Known 4)] @>

module ``For All Specification`` =
    [<Fact>]
    let ``Anything holds true for an empty list``() =
        test <@ [] |> ExList.forall (fun _ -> false) = Known true @>

    [<Fact>]
    let ``Exists holding true gives known true``() =
        test <@ [Exists 1] |> ExList.forall (fun x -> true) = Known true @>

    [<Fact>]
    let ``A case existing that doesn't hold is a known false``() =
        test <@ [Exists 1] |> ExList.forall (fun x -> false) = Known false @>

    [<Fact>]
    let ``A speculative not holding gives unknown``() =
        test <@ [Speculative 1] |> ExList.forall (fun x -> false) = Unknown @>

    [<Fact>]
    let ``Speculative holding true gives known true``() =
        test <@ [Speculative 1] |> ExList.forall (fun x -> true) = Known true @>

module ``For All Awareness Specification`` =
    let forall = ExList.forallAwareness (fun x -> x)
    let restMatching = [Exists (Known true); Speculative (Known true)]

    [<Fact>]
    let ``Anything holds true for an empty list``() =
        test <@ [] |> forall = Known true @>

    [<Fact>]
    let ``Exists holding true gives known true``() =
        test <@ Exists (Known true) :: restMatching |> forall = Known true @>

    [<Fact>]
    let ``A case existing that doesn't hold is a known false``() =
        test <@ Exists (Known false) :: restMatching |> forall = Known false @>

    [<Fact>]
    let ``A speculative not holding gives unknown``() =
        test <@ Speculative (Known false) :: restMatching |> forall = Unknown @>

    [<Fact>]
    let ``Speculative holding true gives known true``() =
        test <@ Speculative (Known true) :: restMatching |> forall = Known true @>

    [<Fact>]
    let ``Any existance of an unknown gives an unknown``() =
        test <@ Exists Unknown :: restMatching |> forall = Unknown @>

    [<Fact>]
    let ``Any speculative unknown gives an unknown``() =
        test <@ Speculative Unknown :: restMatching |> forall = Unknown @>
