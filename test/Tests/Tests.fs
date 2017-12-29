module Tests

open System
open Xunit
open FSharpPlus
open FSharpPlus.Compatibility.Haskell
open Validations
open Validations.Validations // not good

//( # ) :: AReview t b -> b -> t
let seven = 7
let three = 3
let plusOne x = x + 1

[<Fact>]
let testYY() =
  let subject:AccValidation<string,int>  = AccSuccess plusOne <*> AccSuccess seven
  let expected = AccSuccess 8
  Assert.Equal(expected, subject)
[<Fact>]
let testNY() =
  let subject:AccValidation<string list,int>  = AccFailure ["f1"] <*> AccSuccess seven
  let expected = AccFailure ["f1"]
  Assert.Equal(expected, subject)
[<Fact>]
let testYN() =
  let subject:AccValidation<string list,int>  = AccSuccess plusOne <*> AccFailure ["f2"] 
  let expected = AccFailure ["f2"]
  Assert.Equal(expected, subject)
[<Fact>]
let testNN() =
  let subject:AccValidation<string list,int>  = AccFailure ["f1"] <*> AccFailure ["f2"] 
  let expected = AccFailure ["f1";"f2"]
  Assert.Equal(expected, subject)
(*
[<Fact>]
let testValidationNel() =
  let subject  = validation length (const' 0) $ validationNel (Error ())
  Assert.Equal(1, subject)
*)
[<Fact>]
let testEnsureLeftFalse () =
  let subject = ensure three (const' false) (AccFailure seven)
  Assert.Equal((AccFailure seven), subject)

[<Fact>]
let testEnsureLeftTrue () =
  let subject = ensure three (const' true) (AccFailure seven)
  Assert.Equal((AccFailure seven), subject)

[<Fact>]
let testEnsureRightFalse () =
  let subject = ensure three (const' false) (AccSuccess seven)
  Assert.Equal((AccFailure three), subject)

[<Fact>]
let testEnsureRightTrue () =
  let subject = ensure three (const' true ) (AccSuccess seven)
  Assert.Equal((AccSuccess seven), subject)

[<Fact>]
let testOrElseRight () =
  let v = AccSuccess  seven
  let subject = AccValidation.orElse v three
  Assert.Equal(seven, subject)

[<Fact>]
let testOrElseLeft () =
  let v = AccFailure seven
  let subject = AccValidation.orElse v three
  Assert.Equal(three, subject)

//testEnsureLeftFalse, testEnsureLeftTrue, testEnsureRightFalse, testEnsureRightTrue,
//  testOrElseRight, testOrElseLeft
//  :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => Proxy v -> Test


[<Fact>]
let testValidateTrue ()=
  let subject = validate three (const' true) seven
  let expected = AccSuccess seven
  Assert.Equal(expected, subject)

[<Fact>]
let testValidateFalse ()=
  let subject = validate three (const' false) seven
  let expected = AccFailure three
  Assert.Equal(expected, subject)
