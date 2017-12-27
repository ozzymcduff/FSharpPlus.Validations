module Tests

open System
open Xunit
open FSharpPlus
open FsUnit
open Validations

//( # ) :: AReview t b -> b -> t
let seven = 7
let three = 3
let plusOne x = x + 1

[<Fact>]
let testYY() =
  let subject:AccValidation<string,int>  = AccSuccess plusOne <*> AccSuccess seven
  let expected = AccSuccess 8
  Assert.Equal(expected,subject)
[<Fact>]
let testNY() =
  let subject:AccValidation<string list,int>  = AccFailure ["f1"] <*> AccSuccess seven
  let expected = AccFailure ["f1"]
  Assert.Equal(expected,subject)
[<Fact>]
let testYN() =
  let subject:AccValidation<string list,int>  = AccSuccess plusOne <*> AccFailure ["f2"] 
  let expected = AccFailure ["f2"]
  Assert.Equal(expected,subject)
[<Fact>]
let testNN() =
  let subject:AccValidation<string list,int>  = AccFailure ["f1"] <*> AccFailure ["f2"] 
  let expected = AccFailure ["f1";"f2"]
  Assert.Equal(expected,subject)
