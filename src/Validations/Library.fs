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

