{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver -fplugin GHC.TypeLits.Extra.Solver #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The @sized-foldable@ library allows the compiler to reason about
-- the length of foldables.  You'll need in your code the DataKinds
-- and the TypeOperators extension to use it.  It is recommended to
-- use as well PartialTypeSignatures for better type level
-- programming.

module Data.Sized
  ( -- * Types and constructors
    type (...)
  , Infinity
  , Infinite
  , mkSized
  , fromList
  , toList
    -- * Helper functions
  , getCardinality
    -- * Functions operating on sized lists
  , (\\)
  , (++)
  , (<|)
  , take
  , drop
  , all
  , any
  , break
  , and
  , concatMap
  , concat
  , empty
  , cycle
  , tail
  , head
  , last
  , replicate
  , deleteFirstsBy
  , snoc
  , cons
  , insert
  , singleton
  , length
  , reverse
  , filter
  , intersperse
  ) where

import Prelude hiding
  ( concat
  , (++)
  , take
  , drop
  , all
  , any
  , break
  , and
  , concatMap
  , cycle
  , tail
  , head
  , last
  , replicate
  , length
  , reverse
  , filter
  )

import qualified Data.List as L

import Data.Proxy
import GHC.TypeLits
import GHC.TypeLits.Extra

import Data.Type.Equality

{-|
The annotation type for sized foldables.

* @n@: The lower bound of the length

    @
    0 '<=' n
    @

* @m@: The upper bound of the length

    @
    n '<=' m
    @

* @o@: The type which will be contained in the foldable

Example usage:

@
myEmptyListOfInts :: (0'...'0) 'Integer'

aShortList        :: (0'...'4) 'Bool'

aQuadraticMatrix  :: (m'...'m) ((m'...'m) 'Double')

aNonEmptyList     :: (1 '<=' m) => (m'...'n) a
@
-}
newtype (...) (n :: Nat) (m :: Nat) o = Sized [o] deriving (Eq, Ord, Show, Functor)

infixr 0 ...

instance (1 <= a) => Foldable (a...b) where
  foldr f acc (Sized x) = L.foldr f acc x

instance (1 <= a) => Traversable (a...b) where
  traverse f (Sized xs) = Sized <$> traverse f xs

getCardinality
  :: forall m n o.
     ( KnownNat m
     , KnownNat n
     )
  => (m...n) o
  -> (Integer, Integer)
getCardinality _
  = ( natVal (Proxy :: Proxy m)
    , natVal (Proxy :: Proxy n)
    )

{-|
'Infinity' represents a very big 'Nat' (maxBound :: Word64).  It is
only for convenience and not treated like a special value by the
compiler.

Example usage:

@
anArbitraryList :: (0 '...' 'Infinity') a

aStream         :: ('Infinity' '...' 'Infinity') a

allInput :: IO ((0...'Infinity') String)
allInput = fromList . lines <$> getContents
@
-}
type Infinity = 18446744073709551615

{-|
A shorthand for an infinite foldable

Example usage:

@
primes :: 'Infinite' 'Integer'
@
-}
type Infinite = Infinity ... Infinity

{-|
Smart constructor for a sized foldable which checks wether the
given lists length is within the specified bounds.

Example usage:

>>> mkSized [False, True] :: Maybe ((0...3) Bool)
Just |[False,True]| ∈ {0, 1, 2, 3}

>>> mkSized [False, True] :: Maybe ((3...4) Bool)
Nothing
-}
mkSized
  :: forall m n a.
     ( KnownNat m
     , KnownNat n
     , m <= n
     )
  => [a]
  -> Maybe ((m...n) a)
mkSized xs
  | monotonic [natVal (Proxy :: Proxy m), len, natVal (Proxy :: Proxy n)]
    = Just $ Sized xs
  | otherwise = Nothing
    where len = fromIntegral $ L.length xs
          monotonic :: [Integer] -> Bool
          monotonic ms = L.all (uncurry (<=)) $ L.zip ms (L.tail ms)

{-|
Convert any list into a sized foldable without checking bounds.
The compiler knows nothing about its length.

>>> fromList [False,True]
|[False,True]| ∈ {0, 1, ...}
-}
fromList
  :: forall a b.
     KnownNat b
  => [a]
  -> (0...b) a
fromList
  = take . (Sized :: [a] -> (0...b) a)

{-|
Demote a sized foldable to a simple list and forget about its length.

>>> toList (fromList [1,2])
[1,2]
-}
toList
  :: (a...b) c
  -> [c]
toList (Sized x)
  = x

(\\)
  :: Eq a
  => (m...n) a
  -> (o...p) a
  -> ((m-Min m p)...n) a
Sized xs \\ Sized ys
  = Sized $ xs L.\\ ys

infixr 5 <|
(<|) = cons

(++)
  :: (m...n) a
  -> (o...p) a
  -> ((m+o)...(n+p)) a
Sized xs ++ Sized ys
  = Sized $ xs L.++ ys

take
  :: forall p m n a.
     ( KnownNat p
     , p <= n
     )
  => (m...n) a
  -> (Min m p...p) a
take xs
  = Sized
    . L.take (fromIntegral $ natVal (Proxy :: Proxy p))
    $ toList xs

drop
  :: forall d f c a.
     KnownNat (d-f)
  => (c...d) a
  -> ((c - Min c (d-f))...f) a
drop
  = Sized
    . L.drop (fromIntegral $ natVal (Proxy :: Proxy (d-f)))
    . toList

all
  :: (a -> Bool)
  -> (b...c) a
  -> Bool
all f
  = L.all f
    . toList

any
  :: (a -> Bool)
  -> (b...c) a
  -> Bool
any f
  = L.any f
    . toList

break
  :: (a -> Bool)
  -> (m...n) a
  -> ( (0...n) a
     , (0...n) a
     )
break f
  = (\(x,y) -> (Sized x, Sized y))
  . L.break f
  . toList

and :: (a...b) Bool -> Bool
and = L.and . toList

concatMap
  :: (a -> (p...q) b)
  -> (s...t) a
  -> ((s*p)...(t*q)) b
concatMap f xs = concat (fmap f xs)

concat
  :: (m...n) ((o...p) a)
  -> ((m*o)...(n*p)) a
concat = Sized . L.concatMap toList . toList

empty :: (0...0) a
empty = Sized []

cycle
  :: forall m n a o.
     ( KnownNat o
     , 1 <= m
     )
  => (m...n) a
  -> (o...o) a
cycle xs
  = take
    . (Sized :: [a] -> (o...o) a)
    . L.cycle
    $ toList xs

tail
  :: 1 <= m
  => (m...n) a
  -> ((m-1)...(n-1)) a
tail
  = Sized
    . L.tail
    . toList

head
  :: forall m n a.
     1 <= m
  => (m...n) a
  -> a
head
  = L.head
    . toList

last
  :: 1 <= m
  => (m...n) a
  -> a
last
  = L.last
    . toList

replicate
  :: forall m a.
     KnownNat m
  => a
  -> (m...m) a
replicate x
  = Sized $ L.replicate (fromIntegral $ natVal (Proxy :: Proxy m)) x


deleteFirstsBy
  :: (a -> a -> Bool)
  -> (m...n) a
  -> (o...p) a
  -> ((m - Min m p)...n) a
deleteFirstsBy f xs ys
  = Sized $ L.deleteFirstsBy f (toList xs) (toList ys)

snoc
  :: a
  -> (m...n) a
  -> ((m+1)...(n+1)) a
snoc x xs
  = xs ++ singleton x

cons
  :: a
  -> (m...n) a
  -> ((m+1)...(n+1)) a
cons x
  = Sized
    . (x:)
    . toList

insert
  :: Ord a
  => a
  -> (m...n) a
  -> ((m+1)...(n+1)) a
insert x
  = Sized
    . L.insert x
    . toList

singleton
  :: a
  -> (1...1) a
singleton x
  = Sized [x]

length
  :: forall m n a.
     ( KnownNat m
     , KnownNat n
     )
  => (m...n) a
  -> Int
length (Sized xs)
  = case sameNat Proxy Proxy :: Maybe (m :~: n) of
      Just Refl -> fromIntegral $ natVal (Proxy :: Proxy m)
      Nothing -> L.length xs

reverse
  :: (m...n) a
  -> (m...n) a
reverse (Sized xs)
  = Sized $ L.reverse xs

filter
  :: (a -> Bool)
  -> (m...n) a
  -> (0...n) a
filter f (Sized xs)
  = Sized $ L.filter f xs

intersperse
  :: a
  -> (m...n) a
  -> ((Max (m*2) 1 - 1)...(Max (n*2) 1 - 1)) a
intersperse x (Sized xs)
  = Sized $ L.intersperse x xs
