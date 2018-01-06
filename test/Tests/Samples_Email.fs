module Samples_Email
open System
open FSharpPlus
open FSharpPlus.Validations
open FSharpPlus.Operators
open Xunit

// ***** Types *****
type AtString = AtString of string 
type PeriodString = PeriodString of string 
type NonEmptyString = NonEmptyString of string 

type Email = Email of string 

type VError = | MustNotBeEmpty
              | MustContainAt
              | MustContainPeriod

//-- ***** Base smart constructors *****
//-- String must contain an '@' character
//atString :: String -> AccValidation [VError] AtString
let atString (x:string) : AccValidation<VError list,AtString> =
  if String.contains '@' x then _Success <| AtString x
  else _Failure [MustContainAt]

//-- String must contain an '.' character
//periodString :: String -> AccValidation [VError] PeriodString
let periodString (x:string) : AccValidation<VError list,PeriodString> = 
  if String.contains '.' x
  then _Success <| PeriodString x
  else _Failure [MustContainPeriod]

//-- String must not be empty
//nonEmptyString :: String -> AccValidation [VError] NonEmptyString
let nonEmptyString (x:string) : AccValidation<VError list,NonEmptyString> = 
  if not <| String.IsNullOrEmpty x 
  then _Success <| NonEmptyString x
  else _Failure [MustNotBeEmpty]

//-- ***** Combining smart constructors *****
//email :: String -> AccValidation [VError] Email
let email (x:string) : AccValidation<VError list, Email> = 
  result (Email x)   <*
  nonEmptyString x <*
  atString       x <*
  periodString   x

//-- ***** Example usage *****
let success = email "bob@gmail.com"

[<Fact>]
let ``success ``() = Assert.Equal(AccSuccess (Email "bob@gmail.com"), success)

let failureAt = email "bobgmail.com"
[<Fact>]
let ``failureAt ``() = Assert.Equal(AccFailure [MustContainAt], failureAt)

let failurePeriod = email "bob@gmailcom"
[<Fact>]
let ``failurePeriod ``() = Assert.Equal(AccFailure [MustContainPeriod], failurePeriod)


let failureAll = email ""
[<Fact>]
let ``failureAll ``() = Assert.Equal(AccFailure [MustNotBeEmpty;MustContainAt;MustContainPeriod], failureAll)

