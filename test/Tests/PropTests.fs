module PropTests

open System
open Xunit
open FSharpPlus
//open FSharpPlus.Compatibility.Haskell
open Validations
open Validations.Validations // not good
open FsCheck
open FsCheck.Xunit

//let x y= Gen. y
let genAccValidation (e:Gen<'e>) (a:Gen<'a>) : Gen<AccValidation<'e,'a>> = 
  Gen.oneof [Gen.map AccFailure e; Gen.map AccSuccess a]

let testGen : Gen<AccValidation<string list, int>> =
  let strings = Gen.listOf Arb.generate<string>
  genAccValidation strings Arb.generate<int>

let mkAssoc (f:AccValidation<string list, int> -> AccValidation<string list, int> -> AccValidation<string list, int>) :Property=
  let g = Prop.forAll (Arb.fromGen( testGen))
  //  assoc = \x y z -> ((x `f` y) `f` z) === (x `f` (y `f` z))
  let assoc = fun x y z -> (f (f x y) z) = (f x (f y z))
  g assoc 
  //in  property $ join (liftA3 assoc g g g)

[<Fact>]
let prop_semigroup ()= (mkAssoc (fun x y-> AccValidation<_,_>.Append(x,y))).QuickCheckThrowOnFailure()

[<Fact>]
let prop_monoid_assoc() = (mkAssoc plus).QuickCheckThrowOnFailure()
[<Fact>]
let prop_monoid_left_id() =
  let g = Prop.forAll (Arb.fromGen( testGen))
  let empty = getEmpty()
  let check= fun x -> (plus empty x) = x
  (g check).QuickCheckThrowOnFailure()
[<Fact>]
let prop_monoid_right_id ()=
  let g = Prop.forAll (Arb.fromGen( testGen))
  let empty = getEmpty()
  let check= fun x -> (plus x empty) = x
  (g check).QuickCheckThrowOnFailure()

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
module ApplicativeP=  
  ///The identity law
  [<Property>]
  let ``result id <*> v = v``(v :AccValidation<string list, int>) =
    result id <*> v = v
  [<Property>]
  let ``result (<<) <*> u <*> v <*> w = u <*> (v <*> w)``(v :AccValidation<string list, string->string>) (u :AccValidation<string list, string->string>) (w :AccValidation<string list, string>) =
    (result (<<) <*> u <*> v <*> w) = (u <*> (v <*> w))
  ///Homomorphism:
  [<Property>]
  let ``result f <*> result x = result (f x)``(x :AccValidation<string list, int>) (f:AccValidation<string list, int> -> int) =
    let y=(result (f x)):AccValidation<string list, int>
    y=(result f <*> result x) 
  /// Interchange
  /// in haskell: u <*> pure y = pure ($ y) <*> u
  [<Property>] 
  let ``u <*> result y = result ((|>) y) <*> u``(u:AccValidation<string list, string->int> ) (y:string) =
    let right_side =result ((|>) y) <*> u
    let left_side = u <*> (result y)
    right_side=left_side

module AlternativeP=  
  [<Property>]
  let ``empty <|> x = x``(x :AccValidation<string list, int>) =
    getEmpty() <|> x = x
  
  [<Property>]
  let ``x <|> empty = x``(x :AccValidation<string list, int>) =
    x <|> getEmpty() = x
  
  [<Property>]
  let ``(x <|> y) <|> z = x <|> (y <|> z)``(x :AccValidation<string list, int>) (y :AccValidation<string list, int>) (z :AccValidation<string list, int>)=
    ((x <|> y) <|> z) = (x <|> (y <|> z))
  
  [<Property>]
  let ``f <!> (x <|> y) = (f <!> x) <|> (f <!> y)``(x :AccValidation<string list, int>) (y :AccValidation<string list, int>) (f:int->string)=
    (f <!> (x <|> y)) = ((f <!> x) <|> (f <!> y))
  
  //Right Distribution: does not hold
  //[<Property()>]
  let ``(f <|> g) <*> x = (f <*> x) <|> (g <*> x)``(x :AccValidation<string list, int>) (y :AccValidation<string list, int>) (f:AccValidation<string list,int->string>) (g:AccValidation<string list,int->string>)=
    ((f <|> g) <*> x) = ((f <*> x) <|> (g <*> x))
  //[<Property()>]
  let ``S1:(f <|> g) <*> x = (f <*> x) <|> (g <*> x)``(x :AccValidation<string list, int>) (y :AccValidation<string list, int>) (f:int->string) (g:int->string)=
    let f' = AccSuccess f
    let g' = AccSuccess g
    ((f' <|> g') <*> x) = ((f' <*> x) <|> (g' <*> x))

  // holds when f is a function (success)
  [<Property>]
  let ``empty <*> f = empty ``(f:string->int)=
    let empty:AccValidation<string list,_>=getEmpty()
    (empty <*> (AccSuccess f))=getEmpty()

module TraversableP=
(*
  [<Property>]
  let ``t << traverse f = traverse (t << f) ``(x :AccValidation<string list, int>) (t :int->string) (f:string->int)=
    let right_side =((traverse (t << f) x))
    let left_side =(t << traverse f x)
    left_side = right_side
*)
//    t << traverse f = traverse (t << f) 

//    traverse Identity = Identity
//    traverse (Compose << fmap g . f) = Compose << fmap (traverse g) << traverse f
  let x=()
