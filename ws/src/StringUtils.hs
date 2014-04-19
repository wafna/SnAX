module StringUtils where

import qualified Data.ByteString.Lazy.Char8 as BC (ByteString, pack, unpack)
import qualified Data.ByteString as BW (ByteString, pack, unpack, concat)
import qualified Data.ByteString.Lazy as BL

import Data.Char

bwToBC :: BW.ByteString -> BC.ByteString
bwToBC = BC.pack . (fmap $ chr . fromIntegral) . BW.unpack

bcToBW :: BC.ByteString -> BW.ByteString
bcToBW = BW.pack . (fmap $ fromIntegral . ord) . BC.unpack

sToBW :: String -> BW.ByteString
sToBW = BW.pack . (fmap $ fromIntegral . ord)

sToBC :: String -> BC.ByteString
sToBC = BC.pack

bwToS :: BW.ByteString -> String
bwToS = (fmap $ chr . fromIntegral) . BW.unpack

bcToS :: BC.ByteString -> String
bcToS = BC.unpack

blToBC :: BL.ByteString -> BC.ByteString
blToBC = BC.pack . (fmap $ chr . fromIntegral) . BL.unpack

blToBW :: BL.ByteString -> BW.ByteString
blToBW = BW.pack . (fmap fromIntegral) . BL.unpack