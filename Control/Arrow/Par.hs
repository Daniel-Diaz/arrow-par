
-- | Specification of parallel computations via arrows.
--
-- The 'Arrow' class provides methods to describe
-- computations as combinations of functions.
-- Of particular attention to this
-- module are the methods '***' (product) and '&&&' (fanout)
-- of the 'Arrow' class.
--
-- The /product/ method takes two arrows @a ~> b@ and
-- @c ~> d@ and combines them into a single arrow
-- @(a,c) ~> (b,d)@.
--
-- <<http://i.imgur.com/Jlk1LZc.png>>
--
-- From the picture it is easy to see that the computation
-- of /f/ and /g/ can be performed in parallel in /f *** g/.
--
-- The /fanout/ method can be defined in terms of 'arr', '.', and '***'.
--
-- > f &&& g = (f *** g) . arr (\x -> (x,x))
--
-- Therefore, a similar parallelization can be applied.
--
-- The 'ParM' arrow takes advantage of this kind of situations
-- and use a parallel strategy for them. Thus, if you
-- describe the flow of some computation using the 'ParM'
-- arrow, it will be automatically parallelized
-- where possible.
--
-- As an example, consider the following code.
--
-- > foo :: Int -> (Int,Int)
-- > foo = applyPar $ arr f &&& arr g
-- >   where
-- >     sum' = foldl' (+) 0
-- >     f n  = sum' [1 .. n]
-- >     g n  = sum' [2 .. n]
--
-- After compiling the code above passing the @-threaded@ flag
-- and running it using @+RTS -N2@ (in the case that you enjoy
-- at least two processors), both @f@ and @g@ will be applied
-- in parallel for any input of @foo@.
--
-- In some cases, you may not want to parallelize. For these
-- use the alternatives described in /Not-parallel combinators/.
--
-- It is important to know that "Control.Arrow" defines the following rewrite rules (see
-- <http://www.haskell.org/ghc/docs/latest/html/users_guide/rewrite-rules.html>):
--
-- > "product/arr"   forall f g .
-- >                 arr f *** arr g = arr (f *** g)
-- > "fanout/arr"    forall f g .
-- >                 arr f &&& arr g = arr (f &&& g)
--
-- Note that if these rules are fired in the 'ParM' arrow, they would break
-- any intention of parallelization.
module Control.Arrow.Par (
    -- * @ParM@ arrow
    ParM , applyParM , arrM
    -- * Not-parallel combinators
    -- | Alternative not-parallel combinators to be applied
    --   where parallelization is not desirable. For example,
    --   for cheap computations.
  , (***-) , (&&&-)
    -- * @Par@ arrow
    -- | If you are only interested in parallelize a function
    --   which do not have any monad involved, use the 'Par'
    --   arrow.
  , Par  , applyPar
    -- * Categories and Arrows
    -- | These re-exports are provided since they
    --   are always used together with this module.
    --   It is recommended to import the Prelude
    --   hiding @id@ and @(.)@ to avoid name conflicts.
  , module Control.Category
  , module Control.Arrow
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad
import Control.Monad.Parallel
import Data.Functor.Identity

-- | Arrow of parallel computations with results
--   inside a monad.
--
--   The definition is equivalent to the 'Kleisli'
--   arrow, but with a different 'Arrow' instance. In particular,
--   the methods '***' and '&&&' compute in parallel.
--
--   The 'Arrow' instance requires the monad to be an
--   instance of the 'MonadParallel' class.
--   'IO' and 'Identity' are some of the already
--   instantiated types.
data ParM m a b = F (a -> m b)

instance Monad m => Category (ParM m) where
  id = F return
  (F f) . (F g) = F (\x -> g x >>= f)

instance (Functor m, MonadParallel m) => Arrow (ParM m) where
  arr f = F (return . f)
  (F f) *** (F g) = F $ \(x,y) -> bindM2 ((return .) . (,)) (f x) (g y)
  f &&& g = (f *** g) . arr (\x -> (x,x))
  first  (F f) = F $ \(x,y) -> flip (,) y <$> f x
  second (F f) = F $ \(x,y) ->      (,) x <$> f y

-- | Not-parallel version of '***' for 'ParM'.
--   It firstly applies the left function, and then the
--   right function.
(***-) :: Monad m => ParM m a b -> ParM m c d -> ParM m (a,c) (b,d)
(F f) ***- (F g) = F $ \(x,y) -> do
  fx <- f x
  gy <- g y
  return (fx,gy)

-- | Not-parallel version of '&&&' for 'ParM'.
--   It firstly applies the left function, and then the
--   right function.
(&&&-) :: Monad m => ParM m a b -> ParM m a c -> ParM m a (b,c)
(F f) &&&- (F g) = F $ \x -> do
  fx <- f x
  gx <- g x
  return (fx,gx)

-- | Lift a function with embedded result to a 'ParM' arrow.
arrM :: (a -> m b) -> ParM m a b
arrM = F

-- | Feed a 'ParM' arrow with an input and return the result.
applyParM :: ParM m a b -> a -> m b
applyParM (F f) x = f x

instance (Functor m, MonadParallel m) => ArrowApply (ParM m) where
  app = F $ \(F f,x) -> f x

instance (Functor m, MonadParallel m) => ArrowChoice (ParM m) where
  left  = (+++id)
  right = (id+++)
  (F f) +++ (F g) = F $ \e ->
    case e of
     Left  x -> Left  <$> f x
     Right x -> Right <$> g x

-- | 'ParM' arrow type applied to the 'Identity' monad.
type Par = ParM Identity

-- | Feed a 'Par' arrow with an input and return the result.
applyPar :: Par a b -> a -> b
applyPar a = runIdentity . applyParM a
