{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module Data.Sized
  ( type (...)
  , Infinity
  , Infinite
  , mkSized
  , fromList
  , toList
  , (:+:)
  , (:-:)
  , (:*:)
  , Min
  , Max
  , (\\)
  , (++)
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
  )

import qualified Data.List as L
import GHC.TypeLits
import Data.Proxy (Proxy(..))

data (...) (n :: Nat) (m :: Nat) o where
  Sized :: (n <= m) => [o] -> (n...m) o

infixr 0 ...

instance Eq o => Eq ((n...m) o) where
  Sized xs == Sized ys = xs == ys

instance Ord o => Ord ((n...m) o) where
  compare (Sized a) (Sized b) = compare a b

instance Show o => Show ((n...m) o) where
  show (Sized a) = show a

instance Functor (a...b) where
  fmap f (Sized x) = Sized $ fmap f x

instance (1 <= a) => Foldable (a...b) where
  foldr f acc (Sized x) = L.foldr f acc x

instance (1 <= a) => Traversable (a...b) where
  traverse f (Sized xs) = Sized <$> traverse f xs

type (∞) = 18446744073709551616 -- One more than maxBound of Word64
type Infinity = (∞) -- This indirection is to avoid showing the number of Infinity
type Infinite = Infinity ... Infinity

-- | Typelevel operators which respect `Infinity`

type family (:+:) a b where
  Infinity :+: _ = Infinity
  _ :+: Infinity = Infinity
  a :+: b = a + b

type family (:-:) a b where
  Infinity :-: _ = Infinity
  a :-: 0 = a
  0 :-: a = TypeError ('Text "Negative length")
  a :-: b = (a-1) :-: (b-1)

type family (:*:) a b where
  0 :*: _ = 0
  _ :*: 0 = 0
  Infinity :*: _ = Infinity
  _ :*: Infinity = Infinity
  a :*: b = a * b

type family Max a b where
  Max Infinity _ = Infinity
  Max _ Infinity = Infinity
  Max a b = If (a <=? b) b a

type family Min a b where
  Min Infinity a = a
  Min a Infinity = a
  Min a b = If (a <=? b) a b

-- | Typelevel utility functions

type family (:==:) (a::Nat) (b::Nat) where
  a :==: a = 'True
  _ :==: _ = 'False

type family If a b c where
  If 'True b _ = b
  If 'False _ c = c

type family (:&&:) a b where
  'True :&&: 'True = 'True
  _ :&&: _ = 'False

type family (:||:) a b where
  a :||: 'False = a
  'False :||: a = a

-- | Exported functions

mkSized
  :: ( KnownNat m
     , KnownNat n
     , m <= n
     )
  => [a]
  -> Maybe ((m...n) a)
mkSized xs
  = res
    where res = if natVal (fst $ f res) <= len
                && len <= natVal (snd $ f res)
                then Just $ Sized xs
                else Nothing
          f :: Maybe ((m...n) a) -> (Proxy m, Proxy n)
          f _ = (Proxy, Proxy)
          len = fromIntegral $ L.length xs

fromList
  :: [a]
  -> (0...Infinity) a
fromList
  = Sized

toList
  :: (a...b) c
  -> [c]
toList (Sized x)
  = x

(\\)
  :: ( Eq a
     , (m:-:Min m p) <= n
     )
  => (m...n) a
  -> (o...p) a
  -> ((m:-:Min m p)...n) a
Sized xs \\ Sized ys
  = Sized $ xs L.\\ ys

(++)
  :: ( CmpNat m Infinity ~ LT
     , (m:+:o) <= (n:+:p)
     )
  => (m...n) a
  -> (o...p) a
  -> ((m:+:o)...(n:+:p)) a
Sized xs ++ Sized ys
  = Sized $ xs L.++ ys

take
  :: ( KnownNat p
     , p <= n
     , o <= p
     , o <= m
     , If (p <=? m) (o ~ p) (o ~ m)
     )
  => (m...n) a
  -> (o...p) a
take xs
  = res
    where res = Sized
                . L.take (fromIntegral . natVal $ right res)
                $ toList xs
          right :: (o...p) a -> Proxy p
          right _ = Proxy

drop
  :: ( KnownNat (Max (c:-:e) (d:-:f))
     , e <= c
     , f <= d
     , e <= f
     , If (d :==: Infinity)
          (d~f)
          (If (e :==: 0)
              ((f:-:e) <= (d:-:c))
              ((f:-:e) ~ (d:-:c)))
     )
  => (c...d) a
  -> (e...f) a
drop
  = f
    where f = Sized
            . L.drop (fromIntegral . natVal $ right f)
            . toList
          right :: ((o...p) a -> (x...y) a) -> Proxy (Max (o:-:x) (p:-:y))
          right _ = Proxy

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
  :: (s:*:p) <= (t:*:q)
  => (a -> (p...q) b)
  -> (s...t) a
  -> ((s:*:p)...(t:*:q)) b
concatMap f xs = concat (fmap f xs)

concat
  :: (m:*:o) <= (n:*:p)
  => (m...n) ((o...p) a)
  -> ((m:*:o)...(n:*:p)) a
concat = Sized . L.concatMap toList . toList

empty :: (0...0) a
empty = Sized []

cycle
  :: 1 <= m
  => (m...n) a
  -> Infinite a
cycle
  = Sized
    . L.cycle
    . toList

tail
  :: (m:-:1) <= (n:-:1)
  => (m...n) a
  -> ((m:-:1)...(n:-:1)) a
tail
  = Sized
    . L.tail
    . toList

head
  :: 1 <= m
  => (m...n) a
  -> a
head
  = L.head
    . toList

last
  :: ( 1 <= m
     , CmpNat n Infinity ~ LT
     )
  => (m...n) a
  -> a
last
  = L.last
    . toList

replicate
  :: KnownNat m
  => a
  -> (m...m) a
replicate x
  = res
    where res = Sized $ L.replicate (fromIntegral . natVal $ num res) x
          num :: (m...m) a -> Proxy m
          num _ = Proxy

deleteFirstsBy
  :: (m :-: Min m p) <= n
  => (a -> a -> Bool)
  -> (m...n) a
  -> (o...p) a
  -> ((m :-: Min m p)...n) a
deleteFirstsBy f xs ys
  = Sized $ L.deleteFirstsBy f (toList xs) (toList ys)

snoc
  :: ( CmpNat m Infinity ~ LT
     , (m:+:1) <= (n:+:1)
     )
  => a
  -> (m...n) a
  -> ((m:+:1)...(n:+:1)) a
snoc x xs
  = xs ++ singleton x

cons
  :: (m:+:1) <= (n:+:1)
  => a
  -> (m...n) a
  -> ((m:+:1)...(n:+:1)) a
cons x
  = Sized
    . (x:)
    . toList

insert
  :: ( Ord a
     , (m:+:1) <= (n:+:1)
     )
  => a
  -> (m...n) a
  -> ((m:+:1)...(n:+:1)) a
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
  :: ( KnownNat m
     , KnownNat n
     )
  => (m...n) a
  -> Int
length a@(Sized xs)
  = if natVal lower == natVal upper
       then fromIntegral (natVal lower)
       else L.length xs
    where f :: (m...n) a -> (Proxy m, Proxy n)
          f _ = (Proxy, Proxy)
          (lower, upper) = f a
