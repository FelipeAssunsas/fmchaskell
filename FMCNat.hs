{-# LANGUAGE GADTs #-}

module ExNat where

import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

data Nat where
  O :: Nat
  S :: Nat -> Nat

instance Show Nat where
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where
    O    == O    = True
    (S m) == (S n) = m == n
    _    == _    = False

instance Ord Nat where
    O    <= _    = True
    (S m) <= O    = False
    (S m) <= (S n) = m <= n

    min O _ = O
    min _ O = O
    min (S m) (S n) = S (min m n)

    max O n = n
    max m O = m
    max (S m) (S n) = S (max m n)

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd n = not (even n)

(<+>) :: Nat -> Nat -> Nat
O   <+> n = n
(S m) <+> n = S (m <+> n)

monus :: Nat -> Nat -> Nat
m `monus` O   = m
O `monus` _   = O
(S m) `monus` (S n) = m `monus` n

(-*) :: Nat -> Nat -> Nat
(-*) = monus

times :: Nat -> Nat -> Nat
O   `times` _ = O
(S m) `times` n = n <+> (m `times` n)

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

pow :: Nat -> Nat -> Nat
_ `pow` O   = one
m `pow` (S n) = m <*> (m `pow` n)

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

m < n = (m <= n) && not (m == n)

(</>) :: Nat -> Nat -> Nat
_ </> O = error
m </> n
  | m < n     = zero
  | otherwise = one <+> ((m `monus` n) </> n)

(<%>) :: Nat -> Nat -> Nat
_ <%> O = error
n <%> m = n `monus` ((n </> m) <*> m)

eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (n, m) = (n </> m, n <%> m)

(<|>) :: Nat -> Nat -> Bool
O <|> _ = False
m <|> n = (n <%> m) == O

divides :: Nat -> Nat -> Bool
divides = (<|>)

dist :: Nat -> Nat -> Nat
m `dist` n
  | m <= n    = n `monus` m
  | otherwise = m `monus` n

(|-|) :: Nat -> Nat -> Nat
(|-|) = dist

factorial :: Nat -> Nat
factorial O = one
factorial (S n) = (S n) <*> factorial n

sg :: Nat -> Nat
sg O = zero
sg _ = one

lo :: Nat -> Nat -> Nat
lo b a
  | b <= one || a < b = zero
  | otherwise         = one <+> lo b (a </> b)

toNat :: Integral a => a -> Nat
toNat x
  | x < 0     = error
  | x == 0    = O
  | otherwise = S (toNat (x - 1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n

(<->) :: Nat -> Nat -> Nat
(<->) = monus

instance Num Nat where
    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = error
      | x == 0    = O
      | otherwise = S (fromInteger (x - 1))

