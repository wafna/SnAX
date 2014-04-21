{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Prelude hiding (id)

--import Data.String
import Data.Vector as V (toList)
import Data.Acid.Remote
import Data.Int
import Data.Scientific
import Data.Map as Map
import Data.Foldable()
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception (catch, IOException)
import Control.Applicative

--import qualified Data.ByteString as BW (ByteString, concat)
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

import Data.Aeson (eitherDecode, encode)
-- this has to be imported to make the ToJSON derivations work.
import Data.Aeson.Types
-- import Data.Aeson.TH (deriveJSON)

import Network


import SnAXCommon
import StringUtils

type RStr = String

-- Request

data RPC = RPC { name :: RStr, params :: Array }
  deriving (Show)

instance FromJSON RPC where
  parseJSON (Object v) = RPC <$> v .: "name" <*> v .: "params"
  parseJSON _ = mzero

-- orphan ToJSON instances for the domain objects

instance ToJSON User where
  toJSON (User (UserId i) (UserName n)) = object ["id" .= i, "name" .= n]

instance ToJSON UserId where
  toJSON (UserId i) = object ["userId" .= i]

instance ToJSON UserName where
  toJSON (UserName n) = object ["userName" .= n]

instance ToJSON Message where
  toJSON (Message (MessageId mid) (MessageOwner (UserId oid)) rs t c) =
    object ["id" .= mid, "owner" .= oid, "recipients" .= (fmap (\ (MessageRecipient (UserId id)) -> id) rs), "time" .= t, "content" .= c]

instance ToJSON MessageId where
  toJSON (MessageId i) = object ["messageId" .= i]

instance ToJSON MessageTime where
  toJSON (MessageTime t) = object ["time" .= ((truncate t) :: Int64)]

data REither = RSuccess Value | RFailure String

instance ToJSON REither where
  toJSON re = case re of
    RSuccess v -> object ["success" .= v]
    RFailure v -> object ["failure" .= v]

main :: IO ()
main = do
  db <- openRemoteState verifySharedSecret "localhost" (PortNumber 8001)
  quickHttpServe $ route 
    [ ("", (serveDirectoryWith defaultDirectoryConfig "web"))
    , ("api", method POST $ api db)]
  where
  verifySharedSecret :: CommChannel -> IO ()
  verifySharedSecret _ = return ()


{-
Expects requests made like this
  $.ajax({
     type: 'POST',
     data: JSON.stringify(data),
     dataType: 'json'
  });

The payload will look like this:
{ tag1: {name: 'function', params: [...] }
, tag2: {name: 'anotherFunction', params: [...] }
}

The result will look like this:
{ tag1: {"success/failure": data/reason }
, tag2: {"success/failure": data/reason }
}

Note that success simply means the call returned normally.  The call may still not have produced the desired effect (e.g. the id was not found).
In those cases, the function specifies a success return value to indicate this. E.g. a request to delete a message where no message with that id 
exists will return success but the success value may have a bool indicating whether anything was actually deleted.  The reason for all this is 
that we want to wrap all the error handling on the client and only map actual return values down to the call sites.

The tags are client defined and are simply used to keep track of which returned data belong to which call.
-}
api :: ADB -> Snap ()
api db = do
  body <- readRequestBody 100000
  case (eitherDecode $ blToBC body) :: Either String (Map String RPC) of
    Left e -> writeJSON $ RFailure $ "Malformed input: cannot parse JSON: " ++ e
    Right calls -> do
      r <- liftIO $ catch (mapRPC calls db >>= return . Right) $
        \ e -> return $ Left $ show (e :: IOException)
      case r of 
        Left e -> writeJSON $ RFailure e
        Right s -> writeJSON $ RSuccess $ toJSON s
  where
  writeJSON = writeBS . bcToBW . encode

-- Convert a map of RPC calls to an map of return values.
mapRPC :: Map String RPC -> ADB -> IO (Map String Value)
mapRPC m db = (mapRPC' $ Map.toList m) >>= return . Map.fromList
  where
  mapRPC' :: [(String, RPC)] -> IO [(String, Value)]
  mapRPC' fs = case fs of
      (t, f) : xs -> do
        r <- rpc db f
        rs <- mapRPC' xs
        return $ (t, toJSON r) : rs
      [] -> return []

rpc :: ADB -> RPC -> IO REither
rpc db f = 
  let
    fn = name f
    ps = V.toList $ params f
    doink m = return $ RFailure $ "Function: " ++ fn ++ ": " ++ m
    score :: ToJSON a => a -> IO REither
    score = return . RSuccess . toJSON
  in
  case fn of
    "createUser" -> case ps of
      n : [] -> case n of
        String uname -> createUser uname db >>= score
        _ -> doink $ "expected String got " ++ show n
      _ -> doink $ "expected (name)"
    "deleteUser" -> case ps of
      (Number i) : [] -> deleteUser (UserId $ sciToInt i) db >>= score
      _ -> doink "expected (id)"
    "getUserByName" -> case ps of
      (String i) : [] -> getUserByName i db >>= score
      _ -> doink "expected (name)"
    "getUserById" -> case ps of
      (Number i) : [] -> getUserById (UserId $ sciToInt i) db  >>= score
      _ -> doink "expected (name)"
    "listUsers" -> case ps of 
      [] -> listUsers db >>= score
      _ -> doink "no parameters expected."
    "createMessage" -> case ps of
      (Number o) : rs : (String c) : [] -> case arrayToInt rs of
        Left e -> doink $ "invalid recipients list: " ++ e
        Right recips -> createMessage (UserId $ sciToInt o) (fmap UserId recips) c db >>= score
      _ -> doink $ "invalid parameters: " ++ show ps
    "deleteMessage" -> case ps of
      (Number i) : [] -> deleteMessage (MessageId $ sciToInt i) db >>= score
      _ -> doink "expected (id)"
    "listMessagesSent" -> case ps of
      (Number i) : [] -> listMessagesSent (UserId $ sciToInt i) db >>= score
      _ -> doink "expected (name)"
    "listMessagesReceived" -> case ps of
      (Number i) : [] -> listMessagesReceived (UserId $ sciToInt i) db >>= score
      _ -> doink "expected (name)"
    _ -> doink $ "unknown function."
  where
  sciToInt :: Scientific -> Int64
  sciToInt = fromInteger . truncate
  arrayToInt :: Value -> Either String [Int64]
  arrayToInt array = case array of 
    (Array a) -> a2i [] $ V.toList a
    _ -> Left $ concat ["Not an array: ", show array]
    where
    a2i :: [Int64] -> [Value] -> Either String [Int64]
    a2i is vs = case vs of
      [] -> Right $ Prelude.reverse is
      (x : xs) -> case x of
        (Number i) -> a2i (sciToInt i : is) xs
        _ -> Left $ concat ["Not a number: ", show x]

