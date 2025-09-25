-- name: Felipe Assunção
-- id:

module FMCBabyNat where
import GHC.Num (naturalBit)

data BabyNat = O | S BabyNat deriving (Show,Eq)

zero = O
one = S O
two = S(S O)
three = S two
four = S three
five = S four
six = S five
seven = S six
eight = S seven
nine = S eight
ten = S nine




-- This definition says that a BabyNat can be one of two things:
-- 1. The constructor "O" (representing zero)
-- 2. The constructor "S" applied to another BabyNat (representing the successor function)

-- For example, the number 3 would be represented as:

-- The number 0 is just O.
-- The number 1 is S O.
-- The number 2 is S (S O).
-- etc.

-- The first part of the exercise is to implement some basic functions on BabyNats.
-- You can use pattern matching to deconstruct the BabyNat.
-- For example, to define a function f:
-- f O = ...
-- f (S n) = ...
-- where in the second case "n" is the BabyNat to which S was applied.

-- TODO: Implement the following functions.
-- You can use "undefined" as a placeholder for the parts you haven't implemented yet.

-- convert a BabyNat to an Integer
babyNatToInteger :: BabyNat -> Integer
babyNatToInteger O = 0
babyNatToInteger (S n) = 1 + babyNatToInteger n

-- convert an Integer to a BabyNat
-- (bonus: throw an error for negative integers)
integerToBabyNat :: Integer -> BabyNat
integerToBabyNat 0 = O
integerToBabyNat n = S (integerToBabyNat (n-1))

-- add two BabyNats
add :: BabyNat -> BabyNat -> BabyNat
add O m = m
add (S n) m = S (add n m)

-- multiply two BabyNats
mult :: BabyNat -> BabyNat -> BabyNat
mult O m = O
mult (S n) m = add m (mult n m)

-- The following are a few test cases.
-- You can run "cabal test" to run them.
-- (I will use a different set of tests to grade your submission)
prop_add_O_r m = add m O == m
prop_add_S_r m n = add m (S n) == S (add m n)
prop_mult_O_r m = mult m O == O
prop_mult_S_r m n = mult m (S n) == add m (mult m n)
