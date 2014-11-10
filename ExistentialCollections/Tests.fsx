﻿#load "Existentialism.fs"

open ExistentialCollections
open System

let startsWithLetter s = 
    match s with
    | "" -> false
    | _ -> Char.IsLetter s.[0]

let optionStringStartsWithLetter fuzzyString = 
    match fuzzyString with
    | Known s -> Known <| (s |> startsWithLetter)
    | Unknown -> Unknown

let list = 
    [ 1, Known "A"
      2, Known "1"
      3, Known "C"
      4, Unknown
      5, Known "A" ]

let exList = list |> ExList.ofList
let filtered = exList |> ExList.filter (snd >> optionStringStartsWithLetter)
let filteredNums = filtered |> ExList.map fst
let grouped = exList |> ExList.groupBy snd
let groupedFiltered = filtered |> ExList.groupBy snd
let total = filteredNums |> ExList.sum
