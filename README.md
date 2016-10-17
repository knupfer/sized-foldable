# sized-foldable
`sized-foldable` leverages the compilers typechecking on the length of foldables.

## Usage

First we'll need some compiler plugins, pragmas and imports.  The
PartialTypeSignatures and NoMonomorphism pragmas are very much
recommended for programming on the type level, albeit not necessary.

```haskell
{-# OPTIONS_GHC

  -fplugin GHC.TypeLits.KnownNat.Solver
  -fplugin GHC.TypeLits.Extra.Solver
  -fplugin GHC.TypeLits.Normalise

#-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DataKinds     #-}

module Main where

import qualified Data.Sized as S
import           Data.Sized (type (...), (<|))
import           GHC.TypeLits
import           GHC.TypeLits.Extra
```

A small entry point into pure functions.

```haskell
main :: IO ()
main = interact g
```

In the following definition, we take the tail of a list, filter by
some criteria, intersperse it with commas and put parentheses around
the list.

Remember that `tail` is a partial function...

```haskell
f = parens
  . S.intersperse ","
  . getMembers
  . S.tail
  where parens     = S.cons "(" . S.snoc ")"
        getMembers = S.filter (`elem` filterList)
        filterList = "A" <| "B" <| "C" <| "D" <| S.empty
```

Let's look at the inferred type signature of this function.  It says
that the minimal length of the input list is 1 (because of `tail`) and
the maximal length is arbitrary.  The resulting list has a minimal
length of 2 (only the parentheses because `getMembers` may return an
empty list) and a more complicated maximal length (minus one for
`tail`, multiplied by two and subtracted by one because of
`intersperse` and added two because of parens).

```haskell
f :: 1 <= n
  => (n...m) String
  -> (2...Max (m*2 - 2) 1 + 1) String
```

Now we'll apply `f` on the input.  Note the smart constructor
`mkSized`, it's the entry point into the compile time tracked lengths.
Its type signature is the only one which we have to specify, all
others are inferred.  We specify that the list of user input has at
least 5 elements and at most 10 elements.  If this isn't true, we'll
complain.

Note that the type signature on `f xs` is inferred and only there for
clarity.

```haskell
g :: String -> String
g c = case mxs of
        Just xs -> concat $ S.toList (f xs :: (2...19) String)
        _       -> "Invalid input"

  where
    mxs :: Maybe ((5...10) String)
    mxs = S.mkSized (lines c)
```

We could have used `NoMonomorphismRestriction` and `OverloadedStrings`
to infer an even more general type for `f` which would enforce that we
can't modify elements of the list.  We would be only left with the
right to remove or to add elements.

```haskell
f :: ( IsString a
     , Eq a
     , 1 <= n
     )
  => (n...m) a
  -> (2...Max (m*2 - 2) 1 + 1) a
```

