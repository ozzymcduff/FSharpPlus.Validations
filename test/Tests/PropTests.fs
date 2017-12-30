module PropTests

open System
open Xunit
open FSharpPlus
open FSharpPlus.Compatibility.Haskell
open Validations
open Validations.Validations // not good
open FsCheck
open FsCheck.Xunit

//let x y= Gen. y
let genAccValidation (e:Gen<'e>) (a:Gen<'a>) : Gen<AccValidation<'e,'a>>= 
  Gen.oneof $ [Gen.map AccFailure e
               Gen.map AccSuccess a]
//testGen :: Gen (AccValidation [String] Int)
let testGen : Gen<AccValidation<string list, int>>=
  let strings = Gen.listOf Arb.generate<string>
  genAccValidation strings Arb.generate<int>

//mkAssoc :: (
// AccValidation [String] Int 
// -> AccValidation [String] Int
// -> AccValidation [String] Int) 
// -> Property
let mkAssoc (f:AccValidation<string list, int> -> AccValidation<string list, int> -> AccValidation<string list, int>) :Property=
  let g = Prop.forAll (Arb.fromGen( testGen))
  //  assoc = \x y z -> ((x `f` y) `f` z) === (x `f` (y `f` z))
  let assoc = fun x y z -> (f (f x y) z) == (f x (f y z))
  g assoc 
  //in  property $ join (liftA3 assoc g g g)

//[<Fact>]
//let prop_semigroup ()= mkAssoc (<>)
//prop_semigroup = mkAssoc (<>)
[<Fact>]
let prop_monoid_assoc() = (mkAssoc mappend).QuickCheck()

module FunctorP=
  [<Property>]
  let ``map id  =  id ``(x :AccValidation<string list, int>) =
    (AccValidation.map id) x =  id x

  [<Property>]
  let ``map (f << g) = map f << map g ``(x :AccValidation<string list, int>) (f:string->int) (g:int->string)=
    (AccValidation.map (f << g)) x = (AccValidation.map f << AccValidation.map g) x
  

module BifunctorP=  
  [<Property>]
  let ``bimap f g = first f << second g``(x :AccValidation<string, int>) (f:string->int) (g:int->string)=
    (bimap f g) x = (first f << second g) x
module AlternativeP=  
  [<Property>]
  let ``empty <|> x = x``(x :AccValidation<string list, int>) =
    let empty = AccValidation<string list,int>.get_Empty()
    empty <|> x = x
  
  [<Property>]
  let ``x <|> empty = x``(x :AccValidation<string list, int>) =
    let empty = AccValidation<string list,int>.get_Empty()
    x <|> empty = x
  
  [<Property>]
  let ``(x <|> y) <|> z = x <|> (y <|> z)``(x :AccValidation<string list, int>) (y :AccValidation<string list, int>) (z :AccValidation<string list, int>)=
    ((x <|> y) <|> z) = (x <|> (y <|> z))
  
  [<Property>]
  let ``f <!> (x <|> y) = (f <!> x) <|> (f <!> y)``(x :AccValidation<string list, int>) (y :AccValidation<string list, int>) (f:int->string)=
    (f <!> (x <|> y)) = ((f <!> x) <|> (f <!> y))
  
  [<Property>]
  let ``(f <|> g) <*> x = (f <*> x) <|> (g <*> x)``(x :AccValidation<string list, int>) (y :AccValidation<string list, int>) (f:AccValidation<string list,int->string>) (g:AccValidation<string list,int->string>)=
    ((f <|> g) <*> x) = ((f <*> x) <|> (g <*> x))


(*
empty <*> f = empty
*)


(*


prop_monoid_left_id :: Property
prop_monoid_left_id =
  property $ do
    x <- forAll testGen
    (mempty `mappend` x) === x

prop_monoid_right_id :: Property
prop_monoid_right_id =
  property $ do
    x <- forAll testGen
    (x `mappend` mempty) === x

*)