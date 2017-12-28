module Tests

open System
open Xunit
open FSharpPlus
open FSharpPlus.Lens
open FsUnit
open Validations

//( # ) :: AReview t b -> b -> t
let seven = 7
let three = 3
let plusOne x = x + 1
[<Fact>]
let testYY() =
  let subject:AccValidation<string,int>  = ( _Success plusOne <*> _Success seven) ^. _AccValidation
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




(*


testValidationNel :: Test
testValidationNel =
  let subject  = validation length (const 0) $ validationNel (Left ())
  in  TestCase (assertEqual "validationNel makes lists of length 1" subject 1)

testEnsureLeftFalse, testEnsureLeftTrue, testEnsureRightFalse, testEnsureRightTrue,
  testOrElseRight, testOrElseLeft
  :: forall v. (Validate v, Eq (v Int Int), Show (v Int Int)) => Proxy v -> Test

testEnsureLeftFalse _ =
  let subject :: v Int Int
      subject = ensure three (const False) (_Failure # seven)
  in  TestCase (assertEqual "ensure Left False" subject (_Failure # seven))

testEnsureLeftTrue _ =
  let subject :: v Int Int
      subject = ensure three (const True) (_Failure # seven)
  in  TestCase (assertEqual "ensure Left True" subject (_Failure # seven))

testEnsureRightFalse _ =
  let subject :: v Int Int
      subject = ensure three (const False) (_Success # seven)
  in  TestCase (assertEqual "ensure Right False" subject (_Failure # three))

testEnsureRightTrue _ =
  let subject :: v Int Int
      subject = ensure three (const True ) (_Success # seven)
  in  TestCase (assertEqual "ensure Right True" subject (_Success # seven))

testOrElseRight _ =
  let v :: v Int Int
      v = _Success # seven
      subject = v `orElse` three
  in  TestCase (assertEqual "orElseRight" subject seven)

testOrElseLeft _ =
  let v :: v Int Int
      v = _Failure # seven
      subject = v `orElse` three
  in  TestCase (assertEqual "orElseLeft" subject three)

testValidateTrue :: Test
testValidateTrue =
  let subject = validate three (const True) seven
      expected = AccSuccess seven
  in  TestCase (assertEqual "testValidateTrue" subject expected)

testValidateFalse :: Test
testValidateFalse =
  let subject = validate three (const True) seven
      expected = AccFailure three
  in  TestCase (assertEqual "testValidateFalse" subject expected)
*)