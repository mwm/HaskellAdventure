-- Defines all the rooms (the world)

module World
( rooms
, startroom
, sword
, coin
, note
, pizza
, parse
, describeInventory
, takeFromRoom
) where

import DataTypes
import Character

-- could have been staQuo
keep :: a -> a
keep a = a

genericItem name value =
    Item name value "" [] Nothing Nothing Nothing ""

genericKey name key =
    Item name 0 "" [] Nothing Nothing Nothing key

startroom = "garden"

-- Lookup table that holds all the rooms
rooms :: [(Key, Room)]
rooms = map (\x -> (roomKey x, x)) -- Make sure to include the roomKey for lookup
    [ Room
        { roomName = "a dark basement" -- "You are in " ++ "a dark basement"
        , roomDescription = "You are in the basement, it is dark and moist." -- Detailed description
        , roomPortals = [("Staircase to house", "house")] -- Where can you go from here
        , roomInventory = [note, sword] -- What is in the room
        -- This key should be reflected in the portals from other rooms [(path_description, roomkey)]
        , roomKey = "basement"
        , roomInteractive = [trapDoor]
        , roomBeings = []
        }
    , Room
        { roomName = "a beautiful garden"
        , roomDescription = "you are outside of the house, the garden is surrounded by fences"
        , roomPortals = [("door to house", "house")]
        , roomInventory = []
        , roomKey = "garden"
        , roomInteractive = []
        , roomBeings = []
        }
    , Room
        { roomName = "a house"
        , roomDescription = ""
        , roomPortals = [("Door to garden", "garden"), ("Stairwell to basement", "basement"), ("Staircase going upstairs", "upstairs")]
        , roomInventory = [coin]
        , roomKey = "house"
        , roomInteractive = []
        , roomBeings = []
        }
    , Room
        { roomName = "a Pizzahut"
        , roomDescription = ""
        , roomPortals = [("Door to basement", "basement")]
        , roomInventory = [coin, pizza]
        , roomKey = "pizzahut"
        , roomInteractive = []
        , roomBeings = []
        }
    , Room
    -- An infinite staircase going upwards, but there is always just one step back again
        { roomName = "a room"
        , roomDescription = ""
        , roomPortals = [("Stairwell to living room", "house"), ("Stairwell going further upstairs", "upstairs")]
        , roomInventory = [trapDoorKey]
        , roomKey = "upstairs"
        , roomInteractive = []
        , roomBeings = []
        }
    -- More rooms may be added here like the above rooms
    -- , Room
    --     { ... }
    ]

-- unlockInteractive is used to create locked doors
unlockInteractive :: Key -> String -> Portal -> (Character -> Room -> Either String Room)
unlockInteractive key door portal character room =
    if hasKey
        then Right (Room (roomName room) (roomDescription room) (roomInventory room) newPortals (roomKey room) newInteractiveList (roomBeings room))
        else Left "You don't have the key"
        
    where removeDoor = filter $ (/=door) . itemName
          newInteractiveList = removeDoor (roomInteractive room)
          newPortals = portal : (roomPortals room)
          hasKey = key `elem` (map itemKey (characterInventory character))

-- Examples
coin = genericItem "Coin" 1
pizza = genericItem "Pizza" 30
note = Item
    { itemName = "note"
    , itemDetails = "Hey! You! You suck!"
    , itemVerbs = ["read"]
    , itemModifiesCharacter = Nothing
    , itemModifiesRoom = Nothing
    , itemModifiesOpponent = Nothing
    , itemValue = 0
    , itemKey = ""
    }
trapDoorKey = genericKey "Key" "key0"
trapDoor = Item
    { itemName = "trapdoor"
    , itemDetails = "A locked trap door"
    , itemVerbs = ["unlock"]
    , itemModifiesCharacter = Nothing
    , itemModifiesRoom = Just (unlockInteractive "key0" "trapdoor" ("Trapdoor going into a dark abyss", "pizzahut"))
    , itemModifiesOpponent = Nothing
    , itemValue = 0
    , itemKey = "key0"
    }
sword = genericItem "Sword" 100

giveToCharacter :: Item -> Character -> Character
giveToCharacter item character =
    Character (characterName character) (characterHp character) (characterMaxHp character) (characterLevel character) (characterEquipped character) items
        where items = item : characterInventory character

takeFromCharacter :: Key -> Character -> Character
takeFromCharacter item character =
    Character (characterName character) (characterHp character) (characterMaxHp character) (characterLevel character) (characterEquipped character) (removeItem $ characterInventory character)
        where removeItem = filter $ (/=item) . itemName

giveToRoom :: Item -> Room -> Room
giveToRoom item room =
    Room (roomName room) (roomDescription room) items (roomPortals room) (roomKey room) (roomInteractive room) (roomBeings room)
        where items = item : roomInventory room

takeFromRoom :: Key -> Room -> Room
takeFromRoom item room =
    Room (roomName room) (roomDescription room) (removeItem $ roomInventory room) (roomPortals room) (roomKey room) (roomInteractive room) (roomBeings room)
        where removeItem = filter $ (/=item) . itemName

modKey :: (Key, a) -> [(Key, a)] -> [(Key, a)]
modKey _ [] = []
modKey (k, v) (x:xs)
    | fst x == k = (k, v) : (modKey (k, v) xs)
    | otherwise  = x : (modKey (k, v) xs)

dropDown :: Key -> Character -> Key -> RoomAlist -> (Character -> Character, Key -> Key, RoomAlist -> RoomAlist, String)
dropDown key character room rooms =
    case item of
         Nothing -> (keep, keep, keep, "No such item")
         Just x -> (takeFromCharacter key, keep, (modKey (room, (giveToRoom x roomInstance))), "Dropped " ++ key)
         where keep = (\x -> x)
               item = getItem (characterInventory character) key 
               roomInstance =  case lookup room rooms of (Just a) -> a

nounPrefix :: String -> String
nounPrefix (first:_)
    | first `elem` vowels = "an"
    | otherwise           = "a"
    where vowels = "aeiouyAEIOUY"

describeInventory :: String -> [Item] -> String -> String
describeInventory prefix items none =
    if null inventory
         then none
         else prefix ++ describe inventory
    where inventory = map itemName items
          describe []     = ""
          describe (x:xs) =
              nounPrefix x ++ " " ++ x ++ postfix xs ++ describe xs
              where postfix xs
                        | length xs == 1 = ", and "
                        | null   xs      = ""
                        | otherwise      = ", "

goTo :: Key -> RoomAlist -> Key -> Key
goTo new rooms current =
    case newroom of
         Nothing -> current
         Just a -> roomKey a
         where newroom = lookup new rooms

getItemDetails :: Character -> String -> String
getItemDetails character itemstr =
    case item of
         Nothing -> "You don't have that"
         Just x -> itemDetails x
         where item = getItem (characterInventory character) itemstr

-- Verbs are created by partially applying String and GameStateMod
-- Since Verb functions only take a string as itemname, we can check for both interactive
-- objects and acquirable items.
makeVerb :: String -> Verb -> Character -> Key -> RoomAlist -> String -> GameStateMod
makeVerb verb verbDo character room rooms itemname
    | not $ itemname `elem` names = (keep, keep, keep, "There is no such object")
    | itemname `elem` doable      = verbDo character room rooms itemname
    | otherwise                   = (keep, keep, keep, "You cannot " ++ verb ++ " that object")
    where items        = characterInventory character ++ roomInteractive roomInstance
          names        = map itemName items
          doable       = map itemName $ filter (elem verb . itemVerbs) items
          roomInstance = case lookup room rooms of (Just a) -> a -- Let me explain... Ok, so it's a bad idea.

readItem :: Verb
readItem = makeVerb "read" foo
    where foo character room rooms itemname = (keep, keep, keep, "It reads: \"" ++ (getItemDetails character itemname) ++ "\"")

-- Used when we know that the Maybe type holds a value
unpackMaybe :: String -> (Maybe a) -> a
unpackMaybe dbg Nothing = error $ "unpackMaybe: No value to unpack (" ++ dbg ++ ")"
unpackMaybe _ (Just a) = a

unlock :: Verb
unlock = makeVerb "unlock" foo
    where foo character room rooms itemname =
              case modRoom character oldroom of
                   Right newRoom -> (keep, keep, (modKey (room, newRoom)), "Unlocked door")
                   Left string -> (keep, keep, keep, string)
                   where modRoom = unpackMaybe "itemModifiesRoom item" $ itemModifiesRoom item
                         oldroom = unpackMaybe "lookup room rooms" $ lookup room rooms
                         item = unpackMaybe ("getItem () " ++ itemname) $ getItem (roomInteractive oldroom) itemname

pickUp :: Key -> Character -> Key -> RoomAlist -> (Character -> Character, Key -> Key, RoomAlist -> RoomAlist, String)
pickUp key character room rooms =
    case item of
         Nothing -> (keep, keep, keep, "No such item")
         Just x -> (giveToCharacter x, keep, modKey (room, (takeFromRoom key roomInstance)), "Picked up " ++ key)
         where keep = (\x -> x)
               item = getItem (roomInventory roomInstance) key 
               roomInstance = case (lookup room rooms) of (Just a) -> a

-- Called by the REPL
parse :: Character -> [String] -> Key -> RoomAlist -> GameStateMod
parse character command roomKey rooms =
    -- $command is split up into words by the Prelude.words function
    case command of
         [] -> (keep, keep, keep, "") -- Empty command, do nothing

         ("go":"to":room:_) -> (keep, goTo room rooms, keep, "Went to " ++ room)
         ("go":room:_) -> (keep, goTo room rooms, keep, "Entered " ++ room)
         ("walk":"to":room:_) -> (keep, goTo room rooms, keep, "Walked to " ++ room)
         ("enter":room:_) -> (keep, goTo room rooms, keep, "Entered " ++ room)

         ("pick":"up":item:_) -> pickUp item character roomKey rooms
         ("take":item:_) -> pickUp item character roomKey rooms
         ("grab":item:_) -> pickUp item character roomKey rooms

         ("drop":item:_) -> dropDown item character roomKey rooms

         ("examine":item:_) -> (keep, keep, keep, getItemDetails character item)

         ("inventory":_) -> (keep, keep, keep, (describeInventory "You have " (characterInventory character) "Nothing in inventory"))

         -- Special verbs used for interaction on specific objects
         ("read":item:_) -> readItem character roomKey rooms item
         ("unlock":object:_) -> unlock character roomKey rooms object
         -- ("æøå":being:_) -> (keep, keep, keep, "The enemy is no match for the æøå, instant death") -- Do this
         -- ("attack":being:_) -> 

         -- Lels
         ("fuck":"this":noun:_) -> (keep, keep, keep, "It's not my fault, blame the idiot who programmed this " ++ noun)
         ("fuck":"this":_) -> (keep, keep, keep, "It's not my fault, blame the idiot who programmed this shit")
         ("fuck":"me":_) -> (keep, keep, keep, "Should i get a motel room, or...?")
         ("fuck":item:_) -> (keep, keep, keep, "No, fuck you")
         ("æøå":_) -> (keep, keep, keep, "Heia Norge!")
         ("ÆØÅ":_) -> (keep, keep, keep, "Heia Norge!")

         (word:_) -> (keep, keep, keep, "I don't know that word")

