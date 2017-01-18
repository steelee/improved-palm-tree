module Nat where

import Prelude hiding (Enum(..), sum)


--
-- * Part 1: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--   
--   >>> pred Zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
--   
pred :: Nat -> Nat
pred Zero = Zero
-- pattern match test if input Nat is the Nat Zero
pred (Succ n) = n
-- pattern match test if input Nat is of form Nat -> Nat
-- helpful because Succ is type Nat -> Nat


-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero Zero = True
-- pattern match test if input Nat is the Nat Zero
isZero (Succ n) = False
-- pattern match test if input Nat is of form Nat -> Nat
-- helpful because Succ is type Nat -> Nat


-- | Convert a natural number to an integer.
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Num t => Nat -> t
-- t must be a number (int, float, etc) and input is type Nat
toInt Zero = 0
-- pattern match test if input Nat is the Nat Zero
toInt (Succ n) = 1 + toInt n
-- pattern match test if input Nat is of form Nat -> Nat
-- helpful because Succ is type Nat -> Nat
-- so it recursivly matches the outside Succ until only Zero remains


-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--   
add :: Nat -> Nat -> Nat
-- takes in two Nat and returns a Nat
add x Zero = x
add x y = add (Succ x) (pred y)


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub x Zero = x
sub x y
 | gt x y == True = sub (pred x) (pred y)
 | otherwise = Zero


-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt :: Nat -> Nat -> Bool
gt x y = toInt x > toInt y


-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult :: Nat -> Nat -> Nat
mult x Zero = Zero
mult Zero y = Zero
mult x y = add x (mult x (pred y))


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--   
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum = undefined


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds = undefined
-- odds = [1,3..] as numbers not nats... oops
-- this will be a infinite comprehension
