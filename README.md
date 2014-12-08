# Existentialism

** *Reasoning about things that might not exist* **

*Note: all naming is still under consideration - still trying to find something better!*

## Intro

This is basically my own little playground for testing out some ideas around how to model things that you might not know, while still allowing you to derive meaningful results from the things you do know.

At the core of this projects there are only a small number of fundamental building blocks:

1. Awareness
2. ExList (Existance List or maybe Existential List, not made up my mind yet)

### Awareness

This is basically an option with more meaning and semantics. For example, you can't add and subtract options, but you can with the awareness type. Also, semantically, *unknown* tells you the reason why it doesn't have a value, where as something could be None for other reasons.

### ExList

With a traditional list, you know the things in the list (because they're listed) and the things not in the list (because it's not there). With the ExList, it adds one extra possibility to those options ... an item *might be* in the list, and I describe that as being **speculative[ly]** in the list.

## Example
Here's a super-simple example to get started: let's pretend you have a list of people...

| Name		| Gender	| Age	|
|------		|----------	|------	|
| Dan		| Male		| 27	|
| Jessie	| ??????	| 22	|
| Alice		| Female	| 29	|
| Betty		| Female	| 35	|

So, we have a list of people, and we know everything except for Jessie's gender. Now, if we want to find all "Male" people we can make the following conclusions:

1. Dan is definitely in the list.
2. Jessie might be in the list.
3. Alice and Betty do not appear in the list.

This could be written in F# as:

    people |> ExList.filterByAwareness (fun person -> person.Gender) (fun gender -> gender = "Male")

This only really scratches the surface, but should give you an idea of what's possible as you extrapolate the concept!

## Naming Ideas

Here's some brainstorming about better names to use. Ideally, I want things which are fairly self-descriptive, read well in the code and aren't too likely to conflict with other domain language.

Currently:

    Awareness = | Known | Unknown

Maybe something like:

 - Discernment?
 - Perception?
 - Knowing?

Currently:

    Existance = | Exists | Speculative

Maybe something like:

 - Certainty = Certain/Uncertain
 - Confidence?
 - Certitude
