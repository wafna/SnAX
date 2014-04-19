{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main (main) where

import Data.Acid
import Data.Acid.Remote (acidServer, CommChannel)
import Data.IxSet

import Control.Exception (bracket)

import Network

import SnapChatCommon

main :: IO ()
main =
  bracket
    (openLocalState $ DB (Table empty 0) (Table empty 0))
    closeAcidState
    (acidServer verifySharedSecret $ PortNumber 8001)
  where
  verifySharedSecret :: CommChannel -> IO Bool
  verifySharedSecret _ = return True
