{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Prelude hiding (id)

import Data.String
import Data.Vector as V (toList)
-- import Data.Text
import Data.Int
import Data.Scientific
import Data.Map as Map
import Data.Foldable()
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception (catch, IOException)
import Control.Applicative

import qualified Data.ByteString as BW (ByteString, concat)
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

import Data.Aeson (eitherDecode, encode)
-- this has to be imported to make the ToJSON derivations work.
import Data.Aeson.Types
-- import Data.Aeson.TH (deriveJSON)

import API
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

main :: IO ()
main = withDB $ \ db -> do
  quickHttpServe $ route 
    [ ("", (serveDirectoryWith defaultDirectoryConfig "web"))
    , ("api", method POST $ api db)]


{-
Expects requests made like this
  $.ajax({
     type: 'POST',
     data: JSON.stringify(rpc),
     dataType: 'json'
  });
-}
api :: ADB -> Snap ()
api db = do
  body <- readRequestBody 100000
  case (eitherDecode $ blToBC body) :: Either String (Map String RPC) of
    Left e -> setUpUsTheBomb 400 "Malformed input." $ BW.concat ["Cannot parse JSON: ", fromString e, "\n", blToBW body]
    Right calls -> do
      --r <- liftIO $ catch (mapRPC (fmap (\ (tag, call) -> (tag, rpc db call)) (Map.toList calls)) >>= return . Right) $
      r <- liftIO $ catch (mapRPC calls db >>= return . Right) $
        \ e -> return $ Left $ show (e :: IOException)
      case r of 
        Left e -> setUpUsTheBomb 500 "Internal Error" $ sToBW e
--        Right s -> writeBS $ bcToBW $ encode s
        Right s -> writeBS $ bcToBW $ encode s

  where
  -- Emit an error message.
  setUpUsTheBomb :: Int -> BW.ByteString -> BW.ByteString -> Snap ()
  setUpUsTheBomb statusCode statusMsg body = do
      logError $ BW.concat [statusMsg, "\n", body]
      modifyResponse $ setResponseStatus statusCode statusMsg
      writeBS body
      getResponse >>= finishWith

-- going into and out of a list seems weak but the IO monad seems to preclude any other way.
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

rpc :: ADB -> RPC -> IO (Either String Value)
rpc db f = 
  let
    fn = name f
    ps = V.toList $ params f
    doink m = return $ Left $ "Function: " ++ fn ++ ": " ++ m
    score :: ToJSON a => a -> IO (Either String Value)
    score = return . Right . toJSON
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
    _ -> Left "Not an array."
    where
    a2i :: [Int64] -> [Value] -> Either String [Int64]
    a2i is vs = case vs of
      [] -> Right $ Prelude.reverse is
      (x : xs) -> case x of
        (Number i) -> a2i (sciToInt i : is) xs
        _ -> Left "Not a number."

