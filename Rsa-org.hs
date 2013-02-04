module Rsa where

import Data.Bits
import Data.Char

rsaEncrypt :: Integer -> Integer -> Integer -> Integer
rsaEncrypt e n plain  = powerMod plain  e n
rsaDecrypt :: Integer -> Integer -> Integer -> Integer
rsaDecrypt d n cipher = powerMod cipher d n

powerMod x exp n = iter exp seq 1
  where
    seq = iterate (\y -> y*y `mod` n) x
    iter 0 _ ret = ret
    iter b (s:ss) ret = let next = if odd b
                                   then ret * s `mod` n
                                   else ret
                        in iter (shiftR b 1) ss next

rsa129encode :: String -> Integer
rsa129encode s = foldl multiply100 0 $ map encode s
  where
    encode ' ' = 0
    encode c = 1 + ord c - ord 'a'
    multiply100 :: Integer -> Int -> Integer
    multiply100 x y = x * 100 + (toInteger y)

rsa129decode :: Integer -> String
rsa129decode n = map decode $ divide100 n
  where
    decode 0 = ' '
    decode x = chr $ x + ord 'a' -1
    divide100 :: Integer -> [Int]
    divide100 n = iter n []
      where
        iter :: Integer -> [Int] -> [Int]
        iter 0 is = is
        iter m is = iter (m `div` 100) $ fromInteger (m `mod` 100) : is

calcD p q e = let l = lcm (p-1) (q-1)
                  d = euclid (1,0,l) (0,1,e)
              in if d >0
                 then d
                 else d + l

euclid f@(x1,y1,z1) s@(x2,y2,0) = y1
euclid f@(x1,y1,z1) s@(x2,y2,z2) = euclid s t
  where
    q = z1 `div` z2
    t = (x1 - q*x2, y1 - q*y2, z1 - q*z2)

-- public key
rsa129n = 114381625757888867669235779976146612010218296721242362562561842935706935245733897830597123563958705058989075147599290026879543541
rsa129e = 9007
-- signature
rsa129signature = 16717861150380844246015271389168398245436901032358311217835038446929062655448792237114490509578608655662496577974840004057020373
-- original prim number
rsa129p = 3490529510847650949147849619903898133417764638493387843990820577
rsa129q = 32769132993266709549961988190834461413177642967992942539798288533

rsa129d = calcD rsa129p rsa129q rsa129e

rsa129message = 200805001301070903002315180419000118050019172105011309190800151919090618010705
-- rsa129message = rsaDecrypt rsa129e rsa129n $ rsaEncrypt rsa129d rsa129n $ rsa129encode mess

mess = "the magic words are squemish ossifrage"
rsa129message' = rsaEncrypt rsa129d rsa129n $ rsa129encode mess

plain = rsa129decode rsa129message

cipher = 96869613754622061477140922254355882905759991124574319874695120930816298225145708356931476622883989628013391990551829945157815154