module Task where

-----------------------------------------------------
-- Serial lazy isprime ------------------------------
isprime :: Integer -> Bool
isprime n = isDivsOf n primes

primes :: [Integer]
primes = 2: 3: filter isprime (interleave [5, 11..] [7, 13..])

isDivsOf :: Integer -> [Integer] -> Bool
isDivsOf k primes | k < 2 = False
                  | otherwise = null $ 
                                filter ((0==).(mod k)) $
                                takeWhile (<= iSqr k) primes
                  



-----------------------------------------------------
type Junta a = a -> a -> a
type Resolve b a = b -> a

junta :: Junta Int
junta = (+)
-- junta a b = 2*a + b

resolve :: Resolve Integer Int
resolve p | isprime p = 1
          | otherwise = 0













-----------------------------------------------------------
-- Utils --------------------------------------------------

iSqr = floor . sqrt . fromIntegral


interleave (x:xs) ys = x:interleave ys xs

