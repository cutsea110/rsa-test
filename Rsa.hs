module Rsa where

data Modulo = Mod Integer Integer deriving (Eq, Show)

instance Num Modulo where
  (Mod n1 m1) * (Mod n2 m2) 
     | m1 == m2 = Mod (n1 * n2 `mod` m1) m1
     | otherwise = error "modulo mismatch"

fromModulo :: Modulo -> Integer
fromModulo (Mod n _) = n

rsaEncrypt :: Integer -> Integer -> Integer -> Integer
rsaEncrypt e n plain = fromModulo $ (Mod plain n) ^ e

rsaDecrypt :: Integer -> Integer -> Integer -> Integer
rsaDecrypt d n cipher = fromModulo $ (Mod cipher n) ^ d
