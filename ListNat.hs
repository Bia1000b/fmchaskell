{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GADTs, OverloadedLists, TypeFamilies #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}

module ListNat where

import Prelude hiding (last,init,tail,min, max,maximum, minimum,(<=),(>),(<),take, drop, enumFromTo, length, elem, sum, product, elem, (++),rem, reverse,(+), (*), (^))
import Nat
import Ordering

type ListNat = [Nat]

length :: ListNat -> Nat
length [] = O
length (x:xs) = S(length(xs))

elem :: Nat -> ListNat -> Bool
elem x [] = False
elem x (y:ys)
    |x==y = True
    |otherwise = elem x (ys)

sum :: ListNat -> Nat
sum [] = O
sum (x:xs) = x + sum(xs)

prod :: ListNat -> Nat
prod [] = S O
prod (x:xs) = x * prod(xs)

(++) :: ListNat -> ListNat -> ListNat
(++) xs [] = xs
(++) xs (y:ys) = y: ((++) xs (ys))

reverse :: ListNat -> ListNat
reverse [] = []
reverse (x:xs) = (++) [x] (reverse xs)

anyEven :: ListNat -> Bool
anyEven [] = False
anyEven (x:xs)
    |ev x = True
    |otherwise = anyEven (xs)

allEven :: ListNat -> Bool
allEven [] = True
allEven (x:xs)
    |ev x = allEven (xs)
    |otherwise = False

anyOdd :: ListNat -> Bool
anyOdd [] = False
anyOdd (x:xs)
    |od x = True
    |otherwise = anyOdd (xs)

allOdd :: ListNat -> Bool
allOdd [] = True
allOdd (x:xs)
    |od x = allOdd (xs)
    |otherwise = False

anyZero :: ListNat -> Bool
anyZero [] = False
anyZero (x:xs)
    |x == O = True
    |otherwise = anyZero (xs)

allZero :: ListNat -> Bool
allZero [] = True
allZero (x:xs)
    |x == O = allZero (xs)
    |otherwise = False

addNat :: Nat -> ListNat -> ListNat
addNat O (x:xs) = (x:xs)
addNat n [] = []
addNat n (x:xs) = (x+n) : (addNat n xs)

multNat :: Nat -> ListNat -> ListNat
multNat (S O) (x:xs) = (x:xs)
multNat n [] = []
multNat n (x:xs) = (x*n) : (multNat n xs)

expNat :: Nat -> ListNat -> ListNat
expNat (S O) (x:xs) = (x:xs)
expNat n [] = []
expNat n (x:xs) = (x^n) : (expNat n xs)

enumFromTo :: Nat -> Nat -> [Nat]
enumFromTo n m
  | n == m    = [m]
  | n < m     = n : enumFromTo (S n) m
  | otherwise = m : enumFromTo (S m) n

enumTo :: Nat -> ListNat
enumTo n
  | n == O   = [O]
  | otherwise = O : enumFromTo (S O) n

take :: Nat -> ListNat -> ListNat
take n [] = []
take O (x:xs) = []
take (S n) (x:xs) = n:(take n xs)

drop :: Nat -> ListNat -> ListNat
drop (S m) (n:ns) = drop m ns
drop O ns = ns

pwAdd :: ListNat -> ListNat -> ListNat
pwAdd (x:xs) [] = (x:xs)
pwAdd [] [] = []
pwAdd (y:ys) (x:xs) = (x + y) : pwAdd (ys) (xs)

pwMult :: ListNat -> ListNat -> ListNat
pwMult (x:xs) [] = []
pwMult [] [] = []
pwMult (y:ys) (x:xs) = (x * y) : pwMult (ys) (xs)

filterEven :: ListNat -> ListNat
filterEven [] = []
filterEven (x:xs)
    |ev x =  x: filterEven (xs)
    |otherwise = filterEven (xs)

filterOdd :: ListNat -> ListNat
filterOdd [] = []
filterOdd (x:xs)
    |od x =  x: filterOdd (xs)
    |otherwise = filterOdd (xs)

minimum :: ListNat -> Nat
minimum [] = 
minimum (x:xs)
    |
    |


--maximum     : ListNat -> Nat
