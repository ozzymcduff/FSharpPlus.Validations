module Samples_Person
open System
open FSharpPlus
open FSharpPlus.Validations
open FSharpPlus.Operators
open Xunit


type Name = { unName : String } 
with static member create s={unName=s}
type Email = { unEmail : String } 
with static member create s={unEmail=s}
type Age = { unAge : int }
with static member create i={unAge=i}

type Person = { name : Name
                email : Email
                age : Age }
with static member create name email age={name=name;email=email;age=age }


type Error = 
           | NameBetween1And50
           | EmailMustContainAtChar
           | AgeBetween0and120

// -- Smart constructors
// mkName :: String -> AccValidation [Error] Name
let mkName s = 
  let l = length s
  in if (l >= 1 && l <= 50)
    then AccSuccess <| Name.create s
    else AccFailure  [ NameBetween1And50 ]


//mkEmail :: String -> AccValidation [Error] Email
let mkEmail s = 
  if String.contains '@' s
    then AccSuccess <| Email.create s
    else AccFailure [ EmailMustContainAtChar ]

//mkAge :: Int -> AccValidation [Error] Age
let mkAge a = 
  if (a >= 0 && a <= 120)
    then AccSuccess <| Age.create a
    else AccFailure [ AgeBetween0and120 ]

//mkPerson :: String -> String -> Int -> AccValidation [Error] Person
let mkPerson pName pEmail pAge =
  Person.create
  <!> (mkName pName)
  <*> (mkEmail pEmail)
  <*> (mkAge pAge)

//-- Examples
//-- Data constructors for `Name`, `Age`, `Email`, and `Person` should not be 
//-- exported to the example code below:

let validPerson = mkPerson "Bob" "bob@gmail.com" 25
[<Fact>]
let ``validPerson ``() = Assert.Equal(AccSuccess ({name = {unName = "Bob"}; email = {unEmail = "bob@gmail.com"}; age = {unAge = 25}}), validPerson)

let badName = mkPerson "" "bob@gmail.com" 25
[<Fact>]
let ``badName ``() = Assert.Equal(AccFailure [NameBetween1And50], badName)

let badEmail = mkPerson "Bob" "bademail" 25
[<Fact>]
let ``badEmail ``() = Assert.Equal(AccFailure [EmailMustContainAtChar], badEmail)

let badAge = mkPerson "Bob" "bob@gmail.com" 150
[<Fact>]
let ``badAge ``() = Assert.Equal(AccFailure [AgeBetween0and120], badAge)

let badEverything = mkPerson "" "bademail" 150
[<Fact>]
let ``badEverything ``() = Assert.Equal(AccFailure [NameBetween1And50;EmailMustContainAtChar;AgeBetween0and120], badEverything)

open FSharpPlus.Lens
let asMaybeGood = validPerson ^? _Success
[<Fact>]
let ``asMaybeGood ``() = Assert.Equal(Some ({name = {unName = "Bob"}; email = {unEmail = "bob@gmail.com"}; age = {unAge = 25}}), asMaybeGood)
let asMaybeBad = badEverything ^? _Success
[<Fact>]
let ``asMaybeBad ``() = Assert.Equal(None, asMaybeBad)

let asResultGood = validPerson ^. isoAccValidationResult
[<Fact>]
let ``asResultGood ``() = Assert.Equal(Ok ({name = {unName = "Bob"}; email = {unEmail = "bob@gmail.com"}; age = {unAge = 25}}), asResultGood)

let asResultBad = badEverything ^. isoAccValidationResult
[<Fact>]
let ``asResultBad ``() = Assert.Equal(Error [NameBetween1And50;EmailMustContainAtChar;AgeBetween0and120], asResultBad)

