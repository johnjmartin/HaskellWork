-- Several ways of determining whether a number is a prime.
-- Included on slides but posted here so you can try them and
-- compare times (use :set +s).
-- CISC 260, Winter 2015
-- M. Lamb
module Prime where

-- A very large number to test for primeness
-- (the product of several prime numbers)
bigNum = 2 * 3 * 4 * 5 * 1009 * 27644437

prime1 :: Integer -> Bool
prime1 n = not (hasFactor n 2)
    where
    -- hasFactor n start means n has a factor
    -- in [start..sqrt n]
    hasFactor :: Integer -> Integer -> Bool
    hasFactor n start
        | start > (intRoot n) = False
        | (mod n start == 0) = True
        | otherwise = hasFactor n (start+1)

intRoot n = floor (sqrt (fromIntegral n))

prime2 :: Integer -> Bool 
prime2 n = 
  null [fact | fact <- [2..(intRoot n)], mod n fact == 0]
  
prime3 :: Integer -> Bool 
prime3 n = 
  [fact | fact <- [2..(intRoot n)], mod n fact == 0] == []
  
prime4 :: Integer -> Bool
prime4 n = 
   length [fact | fact <- [2..(intRoot n)], mod n fact == 0]  == 0