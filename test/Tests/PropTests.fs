module PropTests

open System
open Xunit
open FSharpPlus
//open FSharpPlus.Compatibility.Haskell
open Validations
open Validations.Validations // not good
open FsCheck
open FsCheck.Xunit
open FSharpPlus.Data

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

  // holds when f is a function (success)
  [<Property>]
  let ``empty <*> f = empty ``(f:string->int)=
    let empty:AccValidation<string list,_>=getEmpty()
    (empty <*> (AccSuccess f))=getEmpty()

module TraversableP=

  //let y_1 =traverse (fun x -> [0..x]) (AccFailure [1])
(*
  [<Property>]
  let ``Result: t << traverse f = traverse (t << f) ``
    (x :Result<string list,string> ) (t :int->string list) (f:string list->int)=
    let t_f = (t << f)
    let right_side = x |> (Result.traverse (t << f))
    let left_side = x |> (t << Result.traverse f ) 
    left_side = right_side
*)
(*
  [<Property>]
  let ``t << traverse f = traverse (t << f) ``(x :AccValidation<string list, int>) (t :int->string) (f:string->int)=
    let right_side =((traverse (t << f) x))
    let left_side =(t << traverse f x)
    left_side = right_side
*)
  [<Property>]
  let ``traverse Identity = Identity``(x :AccValidation<int list, string>)=
    AccValidation.traverse (Identity) x = Identity x
(*
  [<Property>]
  let ``traverse (Compose << fmap g . f) = Compose << fmap (traverse g) << traverse f``(x :AccValidation<int list, string>) (g :int list->string) (f:string->int list)=
    let y_1 = traverse (Compose << map (g << f))
    let y_2 = Compose << map (traverse g) << traverse f
    y_1 x= y_2 x
*)