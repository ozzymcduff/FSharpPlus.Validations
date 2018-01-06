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
    then _Success <| Name.create s
    else _Failure  [ NameBetween1And50 ]


//mkEmail :: String -> AccValidation [Error] Email
let mkEmail s = 
  if String.contains '@' s
    then _Success <| Email.create s
    else _Failure [ EmailMustContainAtChar ]

//mkAge :: Int -> AccValidation [Error] Age
let mkAge a = 
  if (a >= 0 && a <= 120)
    then _Success <| Age.create a
    else _Failure [ AgeBetween0and120 ]

//mkPerson :: String -> String -> Int -> AccValidation [Error] Person
let mkPerson pName pEmail pAge =
  Person.create
  <!> (mkName pName)
  <*> (mkEmail pEmail)
  <*> (mkAge pAge)

//-- Examples
//-- Data constructors for `Name`, `Age`, `Email`, and `Person` should not be 
//-- exported to the example code below:

//validPerson :: AccValidation [Error] Person
let validPerson = mkPerson "Bob" "bob@gmail.com" 25
//-- AccSuccess (Person {name = Name {unName = "Bob"}, email = Email {unEmail = "bob@gmail.com"}, age = Age {unAge = 25}})

//badName :: AccValidation [Error] Person
let badName = mkPerson "" "bob@gmail.com" 25
//-- AccFailure [NameBetween1And50]

//badEmail :: AccValidation [Error] Person
let badEmail = mkPerson "Bob" "bademail" 25
//-- AccFailure [EmailMustContainAtChar]

//badAge :: AccValidation [Error] Person
let badAge = mkPerson "Bob" "bob@gmail.com" 150
//-- AccFailure [AgeBetween0and120]

//badEverything :: AccValidation [Error] Person
let badEverything = mkPerson "" "bademail" 150
//-- AccFailure [NameBetween1And50,EmailMustContainAtChar,AgeBetween0and120]

//asMaybeGood :: Maybe Person
//let asMaybeGood = validPerson ^? _Success
//-- Just (Person {name = Name {unName = "Bob"}, email = Email {unEmail = "bob@gmail.com"}, age = Age {unAge = 25}})

//asMaybeBad :: Maybe Person
//let asMaybeBad = badEverything ^? _Success
//-- Nothing

//asEitherGood :: Either [Error] Person
//let asEitherGood = validPerson ^. _Either
//-- Right (Person {name = Name {unName = "Bob"}, email = Email {unEmail = "bob@gmail.com"}, age = Age {unAge = 25}})

//asEitherBad :: Either [Error] Person
//let asEitherBad = badEverything ^. _Either
//-- Left [NameBetween1And50,EmailMustContainAtChar,AgeBetween0and120]