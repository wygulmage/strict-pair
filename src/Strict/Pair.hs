{-|
Module: Strict.Pair
Description: strict version of @(,)@
License: PublicDomain
Stability: experimental
Portability: GHC

'Pair' is a strict version of '(,)'; that is, if either of values it contains is a bottom (error or nonterminating), the pair is also a bottom. This module provides an interface equivalent to Data.Tuple.
-}

{-# LANGUAGE DeriveDataTypeable
           , DeriveGeneric
           , BangPatterns
   #-}

module Strict.Pair (
Pair ((:!:)),
fst', snd',
curry', uncurry',
swap',
) where

import Control.Applicative (liftA2)
import Control.DeepSeq
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Traversable
import Data.Functor.Classes
import Data.Semigroup
import Data.Data
-- import Foreign.Storable -- Add a Storable instance once the offset management is airtight.
import Text.Read
import GHC.Read (expectP, paren)
import GHC.Generics (Generic1, Generic)


infix 1 :!:
data Pair a b = !a :!: !b
   deriving (Bounded, Data, Generic, Generic1)

fst' :: Pair a b_ -> a
{-^ Get the first value in a 'Pair'. @fst' (x :!: _) = x@. -}
fst' (x :!: _) = x
{-# INLINE fst' #-}

snd' :: Pair a_ b -> b
{-^ Get the second value in a 'Pair'. @snd' (_ :!: y) = y@. -}
snd' (_ :!: y) = y
{-# INLINE snd' #-}

curry' :: (Pair a b -> c) -> a -> b -> c
{-^ Change a function of one 'Pair' into a function of two values. @curry' f x y = f (x :!: y)@. -}
curry' f x y = f (x :!: y)
{-# INLINE curry' #-}

uncurry' :: (a -> b -> c) -> Pair a b -> c
{-^ Change a function of two values into a function of one 'Pair'. @uncurry' f (x :!: y) = f x y@.
@'curry'' (uncurry' f)@ is equivalent to @\ x y -> x `seq` y `seq` f x y@, not @f@. -}
uncurry' f (x :!: y) = f x y
{-# INLINE uncurry' #-}

swap' :: Pair a b -> Pair b a
swap' (x :!: y) = y :!: x

-- strict :: (a, b) -> Pair a b
-- strict = uncurry (:!:)

-- lazy :: Pair a b -> (a, b)
-- lazy = uncurry' (,)

-- toPair :: (c -> a) -> (c -> b) -> c -> Pair a b
-- toPair f g x = f x :!: g x
-- {-# INLINE toPair #-}

fromPair :: (c -> d -> e) -> (a -> c) -> (b -> d) -> Pair a b -> e
{-^ Consume both elements of one 'Pair'. -}
fromPair f g h (x :!: y) = f (g x) (h y)
{-# INLINE fromPair #-}

fromPair2 ::
   (a3 -> b3 -> c) -> (a1 -> a2 -> a3) -> (b1 -> b2 -> b3) ->
   Pair a1 b1 -> Pair a2 b2 -> c
{-^ Consume two 'Pair's. -}
fromPair2 f g h (u :!: x) (v :!: y) = g u v `f` h x y
{-# INLINE fromPair2 #-}

liftPair2 ::
   (a1 -> a2 -> a3) -> (b1 -> b2 -> b3) ->
   Pair a1 b1 -> Pair a2 b2 -> Pair a3 b3
{-^ @biliftA2@ for 'Pair'  -}
liftPair2 = fromPair2 (:!:)
{-# INLINE liftPair2 #-}


instance Eq2 Pair where
   liftEq2 !p !q = fromPair2 (&&) p q

instance Ord2 Pair where
   liftCompare2 !p !q = fromPair2 (<>) p q

instance Read2 Pair where
   liftReadPrec2 rp1 _ rp2 _ = parens $ readInfix <++ readPrefix
      where
         pairPrec = 1
         readInfix = prec pairPrec $ do
            x <- step rp1
            expectP (Symbol ":!:")
            y <- step rp2
            pure (x :!: y)
         readPrefix = do
            paren $ expectP (Symbol ":!:")
            liftA2 (:!:) (step rp1) (step rp2)
   {-^ As a convenience, liftReadPrec2 will read the prefix form @(:!:) x y@ as well as the infix @x :!: y@. -}
   {-# NOTINLINE liftReadPrec2 #-}

   liftReadListPrec2 = liftReadListPrec2Default

instance Show2 Pair where
   liftShowsPrec2 sp1 _ sp2 _ d (x :!: y) = showParen (d > pairPrec) $
      sp1 (pairPrec + 1) x . showString " :!: " . sp2 (pairPrec + 1) y
      where pairPrec = 1
   {-# NOTINLINE liftShowsPrec2 #-}


instance Bitraversable Pair where
   bitraverse = fromPair (liftA2 (:!:))
   {-# INLINE bitraverse #-}

instance Traversable (Pair c) where
   traverse = fromPair fmap (:!:)
   {-# INLINE traverse #-}


instance (Monoid c)=> Applicative (Pair c) where
   pure = (mempty :!:)
   {-# INLINE pure #-}
   liftA2 = liftPair2 (<>)
   {-# INLINE liftA2 #-}

instance (Monoid c)=> Monad (Pair c) where
   (u :!: x) >>= f = case f x of
      v :!: y -> (u <> v) :!: y
   {-# INLINE (>>=) #-}


instance (Semigroup a, Semigroup b)=> Semigroup (Pair a b) where
   (<>) = liftPair2 (<>) (<>)
   {-# INLINE (<>) #-}
   stimes n = bimap (stimes n) (stimes n)
   {-# INLINE stimes #-}

instance (Monoid a, Monoid b)=> Monoid (Pair a b) where
   mempty = mempty :!: mempty
   {-# INLINABLE mempty #-}

instance NFData2 Pair where
   liftRnf2 = fromPair seq


--- Extremely Boring Instances ---

instance Bifunctor Pair where
   bimap = bimapDefault
   second = fmap
   {-# INLINE second #-}

instance Functor (Pair c) where
   fmap = fmapDefault

instance Bifoldable Pair where
   bifoldMap = bifoldMapDefault

instance Foldable (Pair c_) where
   foldMap = foldMapDefault

instance (Eq c)=> Eq1 (Pair c) where
   liftEq = liftEq2 (==)

instance (Eq a, Eq b)=> Eq (Pair a b) where
   (==) = eq2

instance (Read c)=> Read1 (Pair c) where
   liftReadPrec = liftReadPrec2 readPrec readListPrec
   liftReadListPrec = liftReadListPrecDefault

instance (Read a, Read b)=> Read (Pair a b) where
   readPrec = readPrec1
   readListPrec = readListPrecDefault

instance (Show c)=> Show1 (Pair c) where
   liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show a, Show b)=> Show (Pair a b) where
   showsPrec = showsPrec1

instance (NFData c)=> NFData1 (Pair c) where
   liftRnf = liftRnf2 rnf

instance (NFData a, NFData b)=> NFData (Pair a b) where
   rnf = rnf1
