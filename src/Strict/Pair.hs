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

import Control.Applicative
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
{-^ @(:!:)@ is equivalent to '(,)', except it evaluates its arguments (to weak head normal form): @x :!: 'undefined' = 'undefined' = 'undefined' :!: x@.
-}

fst' :: Pair a _b -> a
{-^ Get the first value in a 'Pair'. @fst' (x :!: _) = x@. -}
fst' (x :!: _) = x

snd' :: Pair _a b -> b
{-^ Get the second value in a 'Pair'. @snd' (_ :!: y) = y@. -}
snd' (_ :!: y) = y

curry' :: (Pair a b -> c) -> a -> b -> c
{-^ Change a function of one 'Pair' into a function of two values. @curry' f x y = f (x :!: y)@. -}
curry' f x y = f (x :!: y)
-- Currently curry' is lazy (curry' f undefined x = f undefined). Should it be @curry' f x y = let !xy = x :!: y in f xy@?

uncurry' :: (a -> b -> c) -> Pair a b -> c
{-^ Change a function of two values into a function of one 'Pair'. @uncurry' f (x :!: y) = f x y@.
@'curry'' (uncurry' f)@ is equivalent to @\ x y -> x `seq` y `seq` f x y@, not @f@. -}
uncurry' f (x :!: y) = f x y

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

fromPair2 ::
   (a3 -> b3 -> c) -> (a1 -> a2 -> a3) -> (b1 -> b2 -> b3) ->
   Pair a1 b1 -> Pair a2 b2 -> c
{-^ Consume two 'Pair's. -}
fromPair2 f g h (u :!: x) (v :!: y) = g u v `f` h x y

liftPair2 ::
   (a1 -> a2 -> a3) -> (b1 -> b2 -> b3) ->
   Pair a1 b1 -> Pair a2 b2 -> Pair a3 b3
{-^ @biliftA2@ for 'Pair'  -}
liftPair2 = fromPair2 (:!:)


instance Eq2 Pair where
   liftEq2 !p !q = fromPair2 (&&) p q

instance Ord2 Pair where
   liftCompare2 !p !q = fromPair2 (<>) p q

instance Read2 Pair where
   liftReadPrec2 rp1 _ rp2 _ = parens $ readInfix <|> readPrefix
      where
         constructor = expectP (Symbol ":!:")
         get1 = step rp1
         go r = liftA2 (:!:) r (step rp2)
         pairPrec = 1
         readInfix = prec pairPrec $ go (get1 <* constructor)
         readPrefix = prec 10 $ paren (parens constructor) *> go get1
   {-^ As a convenience, liftReadPrec2 will read the prefix form @(:!:) x y@ as well as the infix @x :!: y@. -}
   {-# NOTINLINE liftReadPrec2 #-}

   liftReadListPrec2 = liftReadListPrec2Default
   {-# NOTINLINE liftReadListPrec2 #-}

instance Show2 Pair where
   liftShowsPrec2 sp1 _ sp2 _ d (x :!: y) = showParen (d > pairPrec) $
      sp1 (pairPrec + 1) x . showString " :!: " . sp2 (pairPrec + 1) y
      where pairPrec = 1
   {-^ @liftShowsPrec2@ always uses infix form @str1 :!: str2@. -}
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

instance Functor (Pair c) where
   fmap = fmapDefault

instance Bifoldable Pair where
   -- bifoldMap = bifoldMapDefault
   bifoldMap = fromPair (<>)
   {-# INLINE bifoldMap #-}

instance Foldable (Pair _c) where
   foldMap = foldMapDefault
   -- foldMap' = foldMap

instance (Ord c)=> Ord1 (Pair c) where
   liftCompare = liftCompare2 compare
   {-# INLINE liftCompare #-}

instance (Ord a, Ord b)=> Ord (Pair a b) where
   compare = compare2
   {-# INLINE compare #-}

instance (Eq c)=> Eq1 (Pair c) where
   liftEq = liftEq2 (==)
   {-# INLINE liftEq #-}

instance (Eq a, Eq b)=> Eq (Pair a b) where
   (==) = eq2
   {-# INLINE (==) #-}

instance (Read c)=> Read1 (Pair c) where
   liftReadPrec = liftReadPrec2 readPrec readListPrec
   {-# NOTINLINE liftReadPrec #-}
   liftReadListPrec = liftReadListPrecDefault
   {-# NOTINLINE liftReadListPrec #-}

instance (Read a, Read b)=> Read (Pair a b) where
   readPrec = readPrec2
   {-# NOTINLINE readPrec #-}
   readListPrec = readListPrecDefault
   {-# NOTINLINE readListPrec #-}

instance (Show c)=> Show1 (Pair c) where
   liftShowsPrec = liftShowsPrec2 showsPrec showList
   {-# NOTINLINE liftShowsPrec #-}

instance (Show a, Show b)=> Show (Pair a b) where
   showsPrec = showsPrec2
   {-# NOTINLINE showsPrec #-}

instance (NFData c)=> NFData1 (Pair c) where
   liftRnf = liftRnf2 rnf
   {-# INLINE liftRnf #-}

instance (NFData a, NFData b)=> NFData (Pair a b) where
   rnf = rnf2
   {-# INLINE rnf #-}


{- Note: INLINE and NOTINLINE
'Read' and 'Show' methods are marked 'NOTINLINE'. Any performance gained by inlining is likely to be marginal compared to the increase in code size in modules that use those methods.

Other methods that have extra constraints (of 'Eq', 'Eq1', 'Ord', 'Ord1', 'Applicative', 'Monad', 'Traversable', 'bitraversable', 'NFData1', 'NFData') are marked 'INLINE' in the hope that they will specialize to their constraints.
-}
