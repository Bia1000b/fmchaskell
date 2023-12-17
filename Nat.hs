module Nat where

import Prelude hiding ((<=), (<), (+), (*), (^), fib, quot, min, gcd, lcm, div, max, pred, rem, (-), if_then_else, leq, eq, ev, od, isMul3, divides)
import Ordering


(+) :: Nat -> Nat -> Nat
(+) n O = n
(+) n (S m) = S (n + m)

(*) :: Nat -> Nat -> Nat
(*) n O = O
(*) n (S m) = n + (n * m)

(^) :: Nat -> Nat -> Nat
(^) n O = S O
(^) n (S m) = n * (n ^ m)

ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S (S n)) = ev n

od :: Nat -> Bool
od O = False
od (S O) = True
od (S (S n)) = od n

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S(S n)) = fib (S n) + fib n

min :: Nat -> Nat -> Nat
min n O = O
min O m = O
min (S n) (S m) = S (min n m)

max :: Nat -> Nat -> Nat
max n O = n
max O m = m
max (S n) (S m) = S(max n m)