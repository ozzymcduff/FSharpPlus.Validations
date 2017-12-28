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
//[<StructuralEquality>]
//[<StructuralComparison>]
type AccValidation<'err,'a> =
  | AccFailure of 'err
  | AccSuccess of 'a

module AccValidation=
  let inline map (f:'T->'U) x=
      match x with 
      | AccFailure e -> AccFailure e
      | AccSuccess a -> AccSuccess (f a) 
  let inline apply e1' e2' = 
    match e1',e2' with
    | AccFailure e1, AccFailure e2 -> AccFailure (plus e1 e2)
    | AccFailure e1, AccSuccess _  -> AccFailure e1
    | AccSuccess _, AccFailure e2 -> AccFailure e2
    | AccSuccess f, AccSuccess a -> AccSuccess (f a)
  let inline foldr f x = function 
      | (AccSuccess a) -> f a x
      | (AccFailure _) -> x

  let inline traverse f = function 
    | (AccSuccess a) -> AccSuccess (map f a)
    | (AccFailure e) -> AccSuccess (AccFailure e)

  let inline bimap f g = function
    | (AccFailure e) -> AccFailure (f e)
    | (AccSuccess a) -> AccSuccess (g a)

  let inline bifoldr f g x = function 
      |(AccSuccess a) -> g a x
      |(AccFailure e) -> f e x

  let inline bitraverse f g = function 
      | (AccSuccess a) -> AccSuccess (map g a)
      | (AccFailure e) -> AccFailure (map f e)

  let inline bind (f:'T->AccValidation<_,_>) x :AccValidation<_,_>=
      match x with 
      | AccFailure e -> AccFailure e
      | AccSuccess a -> (f a) 



type AccValidation<'err,'a> with

  static member inline (<*>)      (e1':AccValidation<'Monoid,_>, e2':AccValidation<'Monoid,_>) : AccValidation<'Monoid,_> = AccValidation.apply e1' e2'
  static member inline Map        (x : AccValidation<_,_>, f) = AccValidation.map f x
  static member inline Bind       (x, f)     = AccValidation.bind f x
 

module Validations=
  let liftResult (f:('b -> 'e)) : (Result<'a,'b>->AccValidation<'e,'a>) = function | Error e-> AccFailure (f e) | Ok a-> AccSuccess a
  /// | 'liftError' is useful for converting an 'Either' to an 'AccValidation'
  /// when the @Left@ of the 'Either' needs to be lifted into a 'Semigroup'.
  let liftChoice (f:('b -> 'e)) : (Either<'b,'a>->AccValidation<'e,'a>) = either (AccFailure << f) AccSuccess

  /// | 'validate's the @a@ with the given predicate, returning @e@ if the predicate does not hold.
  ///
  /// This can be thought of as having the less general type:
  ///
  /// @
  /// validate :: e -> (a -> Bool) -> a -> AccValidation e a
  /// @
  let validate (e:'e) (p:('a -> bool)) (a:'a) : AccValidation<'e,'a> = if p a then AccSuccess a else AccFailure e

  /// | @v 'orElse' a@ returns @a@ when @v@ is AccFailure, and the @a@ in @AccSuccess a@.
  ///
  /// This can be thought of as having the less general type:
  ///
  /// @
  /// orElse :: AccValidation e a -> a -> a
  /// @
  let inline orElse v a = 
      match v with
      | AccFailure _ -> a
      | AccSuccess x -> x

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
