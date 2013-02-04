{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow (first)
import Crypto.Random
import qualified Codec.Crypto.RSA as RSA
import qualified Crypto.PubKey.OpenSsh as SSH
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import qualified Data.ByteString.Base64 as Base64

fromLazy :: BL.ByteString -> BS.ByteString
fromLazy = BS.pack . BL.unpack
toLazy :: BS.ByteString -> BL.ByteString
toLazy = BL.pack . BS.unpack

genKey :: IO (RSA.PublicKey, RSA.PrivateKey)
genKey = (newGenIO::IO SystemRandom) >>= return . fs . flip RSA.generateKeyPair 1024
  where 
    fs (f, s, _) = (f, s)

encrypt :: RSA.PublicKey -> BL.ByteString -> IO (BL.ByteString, SystemRandom)
encrypt pub plain = do
  g <- newGenIO :: IO SystemRandom
  return $ first (toLazy . Base64.encode . fromLazy) $ RSA.encrypt g pub plain

decrypt :: RSA.PrivateKey -> BL.ByteString -> BL.ByteString
decrypt priv cipher = either BL.pack (RSA.decrypt priv . toLazy) $ Base64.decode $ fromLazy cipher

main :: IO ()
main = do
  (pub,priv) <- genKey
  let epk = SSH.encode (SSH.OpenSshPublicKeyRsa pub "your@example.com")
  putStr "encoded public-key: "
  BS.putStrLn epk
  
  let plain1 = "こんにちわ世界"
  putStr "plain text: "
  BL.putStrLn $ BL.fromString plain1
  
  (e1, _) <- encrypt pub $ BL.fromString plain1
  putStr "direct cipher: "
  BL.putStrLn e1
  putStr "direct plain: "
  BL.putStrLn $ decrypt priv e1

  let Right (SSH.OpenSshPublicKeyRsa pub' _) = SSH.decode epk
  (e2, _) <- encrypt pub' $ BL.fromString plain1
  putStr "indirect cipher: "
  BL.putStrLn e2
  putStr "indirect plain: "
  BL.putStrLn $ decrypt priv e2

  let plain2 = "おいらはすけらー!!"
      s = RSA.sign priv $ BL.fromString plain2
  putStr "sign: "
  BL.putStrLn s
  let v = RSA.verify pub (BL.fromString plain2) s
  putStrLn $ "verify: " ++ show v
