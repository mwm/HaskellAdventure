module Character
( getCharacterInfo
, createCharacter
) where

import DataTypes
import IOUtils

default_character = Character
    { characterName = "Default"
    , characterHp = 100
    , characterMaxHp = 100
    , characterLevel = 0
    , characterInventory = []
    , characterEquipped = Nothing
    }

-- Pattern matching in createCharacter must reflect this value
required_fields = ["Name: "]

createCharacter :: [String] -> Character
createCharacter xs =
    Character { characterName = name
              , characterHp = characterHp default_character
              , characterMaxHp = characterMaxHp default_character
              , characterLevel = characterLevel default_character
              , characterInventory = characterInventory default_character
              , characterEquipped = characterEquipped default_character
              }
              where (name:_) = xs -- Decides what order parameters should be given in

getCharacterInfo = do
    putStrLn "Create a character"
    info <- mapM prompt required_fields
    return $ createCharacter info

