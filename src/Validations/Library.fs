namespace Validations

open FSharpPlus
open FSharpPlus.Lens
open FSharpPlus.Compatibility.Haskell

/// | An @AccValidation@ is either a value of the type @err@ or @a@, similar to 'Either'. However,
/// the 'Applicative' instance for @AccValidation@ /accumulates/ errors using a 'Semigroup' on @err@.
/// In contrast, the @Applicative@ for @Either@ returns only the first error.
///
/// A consequence of this is that @AccValidation@ has no 'Data.Functor.Bind.Bind' or 'Control.Monad.Monad' instance. This is because
/// such an instance would violate the law that a Monad's 'Control.Monad.ap' must equal the
/// @Applicative@'s 'Control.Applicative.<*>'
///
/// An example of typical usage can be found <https://github.com/qfpl/validation/blob/master/examples/src/Email.hs here>.
///
type AccValidation<'err,'a> =
  | AccFailure of 'err
  | AccSuccess of 'a
[<AutoOpen>]
module AccValidation=
  let fmap f = function 
      | AccFailure e -> AccFailure e
      | AccSuccess a -> AccSuccess (f a)

//instance Semigroup err => Apply (AccValidation err) where
  let inline (<*>) e1' e2' = 
    match e1',e2' with
    | AccFailure e1, AccFailure e2 -> AccFailure (plus e1 e2)
    | AccFailure e1, AccSuccess _  -> AccFailure e1
    | AccSuccess _, AccFailure e2 -> AccFailure e2
    | AccSuccess f, AccSuccess a -> AccSuccess (f a)

  //let inline (<*>) = (<.>)

  let inline (<!>) a' x' = 
    match a',x' with
    | AccFailure _ , x -> x
    | AccSuccess a , _ ->AccSuccess a

  let inline foldr f x = function 
      | (AccSuccess a) -> f a x
      | (AccFailure _) -> x

  let inline traverse f = function 
    | (AccSuccess a) -> AccSuccess (fmap f a)
    | (AccFailure e) -> AccSuccess (AccFailure e)

  let bimap f g = function
    | (AccFailure e) -> AccFailure (f e)
    | (AccSuccess a) -> AccSuccess (g a)

  let bifoldr f g x = function 
      |(AccSuccess a) -> g a x
      |(AccFailure e) -> f e x

  let bitraverse f g = function 
      | (AccSuccess a) -> AccSuccess (fmap g a)
      | (AccFailure e) -> AccFailure (fmap f e)

  /// | 'validation' is the catamorphism for @AccValidation@.
  let validation (ec: 'e->'c) (ac:'a->'c) (v:AccValidation<'e,'a>) : 'c = 
    match v with
    | AccFailure e -> ec e
    | AccSuccess a -> ac a
  /// | 'liftError' is useful for converting a 'Result' to an 'AccValidation'
  /// when the @Error@ of the 'Either' needs to be lifted into a 'Semigroup'.
  let liftError (f:'b->'e) = function | Error e'-> AccFailure ( f e') | Ok a'->AccSuccess a'
  (*
  module Either=
    let toEither v = validation Left Right v
    /// | 'liftErrorFromEither' is useful for converting an 'Either' to an 'AccValidation'
    /// when the @Left@ of the 'Either' needs to be lifted into a 'Semigroup'.
    let liftError (f:'b->'e) = function | Left e'-> AccFailure ( f e') | Right a'->AccSuccess a'
  
    //let fromEither v = validation Left Right v
    let fromEither = liftError id
  
    let _AccValidation =
      iso
        fromEither
        toEither
      
    let _Either =
      iso
        (function
          |AccFailure e -> Left e
          |AccSuccess a -> Right a)
        (function
          |Left e -> AccFailure e
          |Right a -> AccSuccess a)
*)

  let inline _AccValidation x = id x

  let inline _Result x=
    iso
      (function
        | AccFailure e -> Error e
        | AccSuccess a -> Ok a)
      (function
        | Error e -> AccFailure e
        | Ok a -> AccSuccess a) x

  /// | 'validate's the @a@ with the given predicate, returning @e@ if the predicate does not hold.
  ///
  /// This can be thought of as having the less general type:
  ///
  /// @
  /// validate :: e -> (a -> Bool) -> a -> AccValidation e a
  /// @
  let inline validate (e:'e) (p:'a->bool) (a:'a) :AccValidation<'e,'a> = failwith "!" 
  (*let validate e p a =
    if p a then _Success # a else _Failure # e
  *)


  /// | Converts from 'Either' to 'AccValidation'.
  let inline fromResult e = liftError id e

  /// | Converts from 'AccValidation' to 'Either'.
  let inline toEither v = validation Left Right v
  let inline toResult v = validation Error Ok v

  /// | @v 'orElse' a@ returns @a@ when @v@ is AccFailure, and the @a@ in @AccSuccess a@.
  ///
  /// This can be thought of as having the less general type:
  ///
  /// @
  /// orElse :: AccValidation e a -> a -> a
  /// @
  let inline orElse v a = 
      match v ^. _AccValidation with
      | AccFailure _ -> a
      | AccSuccess x -> x

  /// | Return the @a@ or run the given function over the @e@.
  ///
  /// This can be thought of as having the less general type:
  ///
  /// @
  /// valueOr :: (e -> a) -> AccValidation e a -> a
  /// @
  let inline valueOr (ea:'e->'a) v :'a = 
    match v ^. _AccValidation with
    | AccFailure e -> ea e
    | AccSuccess a -> a

  /// | 'codiagonal' gets the value out of either side.
  let inline codiagonal v= valueOr id v

  /// | 'ensure' leaves the validation unchanged when the predicate holds, or
  /// fails with @e@ otherwise.
  ///
  /// This can be thought of as having the less general type:
  ///
  /// @
  /// ensure :: e -> (a -> Bool) -> AccValidation e a -> AccValidation e a
  /// @
  let inline ensure (e:'e) (p:'a-> bool) =
    function
    |AccFailure x -> AccFailure x
    |AccSuccess a -> validate e p a

    (*
/// | Run a function on anything with a Validate instance (usually Either)
/// as if it were a function on AccValidation
///
/// This can be thought of as having the type
///
/// @(Either e a -> Either e' a') -> AccValidation e a -> AccValidation e' a'@
//validationed :: Validate v => (v e a -> v e' a') -> AccValidation e a -> AccValidation e' a'
  let validationed f = under _AccValidation f
*)

  /// | @bindValidation@ binds through an AccValidation, which is useful for
  /// composing AccValidations sequentially. Note that despite having a bind
  /// function of the correct type, AccValidation is not a monad.
  /// The reason is, this bind does not accumulate errors, so it does not
  /// agree with the Applicative instance.
  ///
  /// There is nothing wrong with using this function, it just does not make a
  /// valid @Monad@ instance.
  let bindValidation v f = 
    match v with
    | AccFailure e -> AccFailure e
    | AccSuccess a -> f a
(*
-- | An infix alias for 'review'.
--
-- @
-- 'unto' f # x ≡ f x
-- l # x ≡ x '^.' 're' l
-- @
--
-- This is commonly used when using a 'Prism' as a smart constructor.
--
-- >>> _Left # 4
-- Left 4
--
-- But it can be used for any 'Prism'
--
-- >>> base 16 # 123
-- "7b"
--
-- @
-- (#) :: 'Iso''      s a -> a -> s
-- (#) :: 'Prism''    s a -> a -> s
-- (#) :: 'Review'    s a -> a -> s
-- (#) :: 'Equality'' s a -> a -> s
-- @
( # ) :: AReview t b -> b -> t
( # ) p = runIdentity #. unTagged #. p .# Tagged .# Identity
*)
(*
  ( #. ) _  = coerce' 
  ( .# ) p _ = coerce p
l # x ≡ x '^.' 're' l
*)
  /// either expressed in result
  let inline either' f g = function Error x -> f x | Ok y -> g y

  let inline _Failure x1 =   //(prism AccFailure <| either' Ok (Error << AccSuccess)) x
    prism
      (fun x -> _Result Error x)
      (fun x -> match x ^. _Result with
                | Error e -> Ok e
                | Ok a -> Error (_Result Ok a)) x1
  let inline _Success x1= //(prism AccSuccess <| either' Ok (Error << AccFailure)) x
    (prism
      (fun x -> _Result Ok x)
      (fun x -> match x ^. _Result with
                 | Error e -> Error (_Result  Error e)
                 | Ok a -> Ok a)) x1
