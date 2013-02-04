{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow (first, (&&&), (***))
import Crypto.Random
import qualified Codec.Crypto.RSA as RSA
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import qualified Data.ByteString.Base64 as Base64

m :: BL.ByteString
m = BL.fromString "こんにちわ世界!!"

m' :: BL.ByteString
m' = BL.fromString "the magic words are squeamish ossifrage!!"

fromLazy :: BL.ByteString -> BS.ByteString
fromLazy = BS.pack . BL.unpack
toLazy :: BS.ByteString -> BL.ByteString
toLazy = BL.pack . BS.unpack

genkey :: IO (RSA.PublicKey, RSA.PrivateKey)
genkey = (newGenIO::IO SystemRandom) >>= return . fs . flip RSA.generateKeyPair 1024
  where 
    fs (f, s, _) = (f, s)

encrypt :: RSA.PublicKey -> BL.ByteString -> IO (BL.ByteString, SystemRandom)
encrypt pub plain = do
  g <- newGenIO :: IO SystemRandom
  return $ first (toLazy . Base64.encode . fromLazy) $ RSA.encrypt g pub plain

decrypt :: RSA.PrivateKey -> BL.ByteString -> BL.ByteString
decrypt priv cipher = either BL.pack (RSA.decrypt priv . toLazy) $ Base64.decode $ fromLazy cipher

main = BL.putStrLn m

