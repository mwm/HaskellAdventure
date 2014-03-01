module DataTypes
( Item(..)
, Character(..)
, Room(..)
, Key
, Portal
, GameStateMod
, GameState
, Verb
, VerbDo
, RoomAlist
, getItem
, (~=)
, (/~=)
) where

import Data.List (find)
import Data.Char (toLower)

type Key = String
type RoomAlist = [(Key, Room)]
type GameStateMod = (Character -> Character, Key -> Key, RoomAlist -> RoomAlist, String)
type Verb = Character -> Key -> RoomAlist -> String -> GameStateMod
type VerbDo = Character -> Key -> RoomAlist -> String -> GameState
type GameState = (Character, Key, RoomAlist, String)

-- A portal contains a description of where the portal is, and
-- a lookup key for the room alist.
type Portal = (String, Key)

-- Describes the place where the player is
data Room = Room
    { roomName :: String
    , roomDescription :: String
    , roomInventory :: [Item]
    , roomPortals :: [Portal]
    , roomKey :: String
    , roomInteractive :: [Item]
    , roomBeings :: [(String, Being)]
    }

-- To be expanded
data Chat = Chat String deriving (Show)

-- A living thing
data Being = Being
    { beingName :: String
    , beingDescription :: String
    , beingHp :: Int
    , beingInventory :: [Item]
    , beingEquipped :: Maybe Item
    , beingModifiesCharacter :: Character -> Character
    , beingModifiesRoom :: RoomAlist -> Key -> RoomAlist
    , beingInteract :: Maybe Chat
    }

data Opponent = Opponent
    { opponentName :: String
    }

data Item = Item
    { itemName :: String
    , itemValue :: Int
    , itemDetails :: String
    , itemVerbs :: [String]
    , itemModifiesOpponent :: Maybe (Opponent -> Either String Opponent)
    , itemModifiesRoom :: Maybe (Character -> Room -> Either String Room)
    , itemModifiesCharacter :: Maybe (Character -> Room -> Either String Character)
    , itemKey :: Key -- Is not always required
    }

data Character = Character
    { characterName :: String
    , characterHp :: Int
    , characterMaxHp :: Int
    , characterLevel :: Int
    , characterEquipped :: Maybe Item
    , characterInventory :: [Item]
    }

-- Case insensitive string comparison
(~=) :: String -> String -> Bool
s0 ~= s1 = (map toLower s0) == (map toLower s1)
(/~=) :: String -> String -> Bool
s0 /~= s1 = not (s0 ~= s1)

getItem :: [Item] -> String -> Maybe Item
getItem character itemstr = find ((itemstr==) . itemName) character

