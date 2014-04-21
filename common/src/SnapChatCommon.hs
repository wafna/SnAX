{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module SnapChatCommon (ADB, DB(..), Table(..),
    createUser, deleteUser, getUserByName, getUserById, listUsers,
    createMessage, deleteMessage, listMessagesSent, listMessagesReceived,
    User(..), UserId(..), UserName(..),
    Message(..),  MessageId(..), MessageOwner(..), MessageRecipient(..), MessageTime(..)
    ) where

import Prelude hiding (words, concat, id, null)

import Data.Acid

import Control.Monad.State
import Control.Monad.Reader
import Data.SafeCopy

import Data.Int
import Data.Data
import Data.Text hiding (empty, null)
import Data.IxSet
import Data.Typeable()
import Data.Time.Clock.POSIX

------------------------------------------------------
-- the domain.

data User = User 
  { userId :: UserId
  , userName :: UserName 
  } deriving (Eq, Typeable, Show, Data)

newtype UserId = UserId Int64
  deriving (Eq, Ord, Typeable, Show, Data)

newtype UserName = UserName Text
  deriving (Eq, Ord, Typeable, Show, Data)

instance Ord User where
   compare p q = compare (userId p) (userId q)

instance Indexable User where
   empty = ixSet 
      [ ixFun $ \ u -> [userId u]
      , ixFun $ \ u -> [userName u]
      ]

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''UserName)

data Message = Message 
  { messageId :: MessageId
  , messageOwner :: MessageOwner
  , messageRecipients :: [MessageRecipient]
  , messageTime :: MessageTime
  , messageContent :: MessageContent 
  } deriving (Eq, Typeable, Show, Data)

instance Ord Message where
   compare p q = compare (messageId p) (messageId q)

newtype MessageId = MessageId Int64
  deriving (Eq, Ord, Typeable, Show, Data)

newtype MessageOwner = MessageOwner UserId
  deriving (Eq, Ord, Typeable, Show, Data)

newtype MessageRecipient = MessageRecipient UserId
  deriving (Eq, Ord, Typeable, Show, Data)

newtype MessageTime = MessageTime POSIXTime
  deriving (Eq, Ord, Typeable, Show, Data)

type MessageContent = Text

newtype Word = Word Text
  deriving (Eq, Ord, Typeable, Show, Data)

instance Indexable Message where
   empty = ixSet 
      [ ixFun $ \ m -> [messageId m]
      , ixFun $ \ m -> [messageOwner m]
      , ixFun $ \ m -> fmap Word $ words $ messageContent m
      , ixFun messageRecipients
      , ixFun $ \ m -> [messageTime m]
      ]

$(deriveSafeCopy 0 'base ''Message)
$(deriveSafeCopy 0 'base ''MessageTime)
$(deriveSafeCopy 0 'base ''MessageOwner)
$(deriveSafeCopy 0 'base ''MessageId)
$(deriveSafeCopy 0 'base ''MessageRecipient)

---------------------------------------------------------
-- the database.

-- Tables contain an IxSet and a counter for the next id.
data (Indexable a, Typeable a, Ord a) => Table a = Table { set :: IxSet a, nextId :: Int64 }

data DB = DB { users :: Table User, messages :: Table Message }
   deriving (Typeable)

$(deriveSafeCopy 0 'base ''Table)
$(deriveSafeCopy 0 'base ''DB)

-- so clients don't have to import AcidState
type ADB = AcidState DB

------------------------------------------------------
-- the internal api.

createUserDB :: Text -> Update DB (Either Text UserId)
createUserDB name = do 
    db <- get
    let t = users db
    let s = set t
    -- enforce uniqueness of user names
    case null $ s @= (UserName name) of
      True -> do
        let id = nextId t
        let uid = UserId id
        let s' = insert (User uid (UserName name)) s
        put $ db { users = Table s' (1 + id) }
        return $ Right uid
      False -> return $ Left $ concat ["Duplicate name: ", name]

getUserByNameDB :: Text -> Query DB (Maybe User)
getUserByNameDB name = do
    db <- ask
    case toList $ (set $ users db) @= (UserName name) of
        p : [] -> return $ Just p
        _ -> return Nothing

getUserByIdDB :: UserId -> Query DB (Maybe User)
getUserByIdDB id = do
    db <- ask
    case toList $ (set $ users db) @= id of
        p : [] -> return $ Just p
        _ -> return Nothing

deleteUserDB :: UserId -> Update DB Bool
deleteUserDB id = do 
    db <- get
    let t = users db
    let s = set t
    -- enforce uniqueness of user names
    case toList $ s @= id of
        [] -> return False
        _ -> do
            let s' = deleteIx id s
            put $ db { users = t { set = s' } }
            return True

listUsersDB :: Query DB [User]
listUsersDB = do
    db <- ask
    return $ toList $ set $ users db

createMessageDB :: UserId -> [UserId] -> POSIXTime -> Text -> Update DB MessageId
createMessageDB owner recipients time content = do 
    db <- get
    let t = messages db
    let s = set t
    let id = nextId t
    let uid = MessageId id
    -- todo check validity of recipients
    let s' = insert (Message uid (MessageOwner owner) (fmap MessageRecipient recipients) (MessageTime time) content) s
    put $ db { messages = Table s' (1 + id) }
    return uid

deleteMessageDB :: MessageId -> Update DB Bool
deleteMessageDB id = do
    db <- get
    let t = messages db
    let s = set t
    case toList $ s @= id of
        [] -> return False
        _ -> do
            let s' = deleteIx id s
            put $ db { messages = t { set = s' } }
            return True

listMessagesReceivedDB :: UserId -> Query DB [Message]
listMessagesReceivedDB id = do
    db <- ask
    return $ toList $ (set $ messages db) @+ [MessageRecipient id]

listMessagesSentDB :: UserId -> Query DB [Message]
listMessagesSentDB id = do
    db <- ask
    return $ toList $ (set $ messages db) @= (MessageOwner id)

$(makeAcidic ''DB 
  [ 'createUserDB, 'deleteUserDB, 'listUsersDB, 'getUserByIdDB, 'getUserByNameDB
  , 'createMessageDB, 'deleteMessageDB, 'listMessagesSentDB, 'listMessagesReceivedDB
  ])

------------------------------------------------------
-- the external api.

type SnapChatAPI a = ADB -> IO a

createUser :: Text -> SnapChatAPI (Either Text UserId)
createUser name db = update db (CreateUserDB name)

deleteUser :: UserId -> SnapChatAPI Bool
deleteUser id db = update db (DeleteUserDB id)

getUserByName :: Text -> SnapChatAPI (Maybe User)
getUserByName name db = query db (GetUserByNameDB name)

getUserById :: UserId -> SnapChatAPI (Maybe User)
getUserById id db = query db (GetUserByIdDB id)

listUsers :: SnapChatAPI [User]
listUsers db = query db (ListUsersDB)

createMessage :: UserId -> [UserId] -> Text -> SnapChatAPI MessageId
createMessage owner recipients content db = do
    t <- getPOSIXTime
    update db (CreateMessageDB owner recipients t content)

deleteMessage :: MessageId -> SnapChatAPI Bool
deleteMessage id db = update db (DeleteMessageDB id)

listMessagesSent :: UserId -> SnapChatAPI [Message]
listMessagesSent owner db = query db (ListMessagesSentDB owner)

listMessagesReceived :: UserId -> SnapChatAPI [Message]
listMessagesReceived recipient db = query db (ListMessagesReceivedDB recipient)

