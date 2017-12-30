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
    | AccFailure e1, AccFailure e2 -> AccFailure (plus e1 e2) // Plus.Invoke x y
    | AccFailure e1, AccSuccess _  -> AccFailure e1
    | AccSuccess _, AccFailure e2 -> AccFailure e2
    | AccSuccess f, AccSuccess a -> AccSuccess (f a)
  let inline foldr f x = function 
      | (AccSuccess a) -> f a x
      | (AccFailure _) -> x

  let inline traverse f = function 
    | (AccSuccess a) -> map AccSuccess (f a)
    | (AccFailure e) -> AccFailure e

  let inline bimap f g = function
    | (AccFailure e) -> AccFailure (f e)
    | (AccSuccess a) -> AccSuccess (g a)

  let inline bifoldr f g x = function 
      |(AccSuccess a) -> g a x
      |(AccFailure e) -> f e x

  let inline bitraverse f g = function 
      | (AccSuccess a) -> AccSuccess (map g a)
      | (AccFailure e) -> AccFailure (map f e)

  /// | @bindValidation@ binds through an AccValidation, which is useful for
  /// composing AccValidations sequentially. Note that despite having a bind
  /// function of the correct type, AccValidation is not a monad.
  /// The reason is, this bind does not accumulate errors, so it does not
  /// agree with the Applicative instance.
  ///
  /// There is nothing wrong with using this function, it just does not make a
  /// valid @Monad@ instance.
  let inline bind (f:'T->AccValidation<_,_>) x :AccValidation<_,_>=
      match x with 
      | AccFailure e -> AccFailure e
      | AccSuccess a -> f a

  /// | @v 'orElse' a@ returns @a@ when @v@ is AccFailure, and the @a@ in @AccSuccess a@.
  ///
  /// This can be thought of as having the less general type:
  ///
  /// @
  /// orElse :: AccValidation e a -> a -> a
  /// @
  let inline orElse v a = 
      match v with
      |AccFailure _ -> a
      |AccSuccess x -> x
  /// | Return the @a@ or run the given function over the @e@.
  ///
  /// This can be thought of as having the less general type:
  ///
  /// @
  /// valueOr :: (e -> a) -> AccValidation e a -> a
  /// @
  //valueOr :: Validate v => (e -> a) -> v e a -> a
  let valueOr ea v = 
    match v with
    |AccFailure e -> ea e
    |AccSuccess a -> a
  let liftResult (f:('b -> 'e)) : (Result<'a,'b>->AccValidation<'e,'a>) = function | Error e-> AccFailure (f e) | Ok a-> AccSuccess a
  /// | 'liftError' is useful for converting an 'Either' to an 'AccValidation'
  /// when the @Left@ of the 'Either' needs to be lifted into a 'Semigroup'.
  let liftChoice (f:('b -> 'e)) : (Either<'b,'a>->AccValidation<'e,'a>) = either (AccFailure << f) AccSuccess

  let inline alt a x=
      match a,x with 
      | AccFailure _, x' -> x'
      | AccSuccess a', _ -> AccSuccess a'

type AccValidation<'err,'a> with

  // as Applicative
  static member Return            x = AccSuccess x

  static member inline (<*>)      (f:AccValidation<_,'T->'U>, x:AccValidation<_,'T>) : AccValidation<_,_> = 
        AccValidation.apply f x
  // as Functor
  static member Map        (x : AccValidation<_,_>, f) = AccValidation.map f x
  static member Bind       (x, f)     = AccValidation.bind f x
  // bimap
  static member Bimap (x:AccValidation<'T,'V>, f:'T->'U, g:'V->'W) :AccValidation<'U,'W> = AccValidation.bimap f g x
  // 
  static member inline get_Empty () = AccFailure ( getEmpty() )
  //static member Append 
  static member Append (x:AccValidation<_,_>, y:AccValidation<_,_>) = AccValidation.alt x y
  static member Traverse (t:AccValidation<_,'T>, f : 'T->AccValidation<_,'U>) : AccValidation<_,_>=AccValidation.traverse f t

module Validations=

  /// | 'validate's the @a@ with the given predicate, returning @e@ if the predicate does not hold.
  ///
  /// This can be thought of as having the less general type:
  ///
  /// @
  /// validate :: e -> (a -> Bool) -> a -> AccValidation e a
  /// @
  let validate (e:'e) (p:('a -> bool)) (a:'a) : AccValidation<'e,'a> = if p a then AccSuccess a else AccFailure e
  //validationNel :: Either e a -> AccValidation (NonEmpty e) a
  /// | 'validationNel' is 'liftError' specialised to 'NonEmpty' lists, since
  /// they are a common semigroup to use.
  let validationNel (x:Result<_,_>) : (AccValidation<NonEmptyList<'e>,'a>)= (AccValidation.liftResult result) x


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
