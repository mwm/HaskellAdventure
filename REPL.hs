module REPL
( mainloop
, newGame
) where

import DataTypes
import Globals (eol, ps1)
import Character
import IOUtils
import World
import System.Cmd (system)

describePaths :: [Portal] -> String
describePaths paths =
    let rec [] _ = []
        rec (x:xs) n =
            "    " ++ snd x ++ ": " ++ fst x ++ eol ++ rec xs (n+1)
            in "The following paths can be taken from here" ++ eol ++ rec paths 1

describeRoom :: Room -> String
describeRoom room =
    "You are in " ++ name ++ (describeInventory ", you can see " (roomInventory room ++ roomInteractive room) "") ++ eol ++ (describePaths $ roomPortals room)
    where name = roomName room

mainloop :: Character -> Key -> RoomAlist -> String -> IO ()
mainloop character room rooms message = do
    system "clear"
    putStr $ describeRoom $ case lookup room rooms of (Just a) -> a -- This should never return Nothing
    putStrLn message
    input <- prompt ps1
    let (modCharacter, modRoom, modRooms, message) = parse character (words input) room rooms
    mainloop (modCharacter character) (modRoom room) (modRooms rooms) message

-- Create a new game
newGame = do
    character <- getCharacterInfo
    mainloop character startroom rooms ""

