import Control.Monad

import Data.Maybe
import Data.List

import System.IO
import System.Process

data Workspace = Workspace String Int [String] deriving Show
data UserState = UserState Int [Workspace] deriving Show

getDesktopName :: String -> String -> String
getDesktopName wsp dsk = wsp ++ ":" ++ dsk

getDesktopRepr :: String -> (String, String)
getDesktopRepr str = let (workspace, desktop) = span (/= ':') str in (workspace, tail desktop)

getWorkspaceName :: UserState -> String
getWorkspaceName (UserState wIdx wList) =
    let Workspace wName dIdx dList = wList !! wIdx
    in getDesktopName wName (dList !! dIdx)

loop :: UserState -> IO ()
loop cState@(UserState wIdx wList) = do
    main:det <- getLine
    let Workspace wName dIdx dList = wList !! wIdx
    newState <- case main of
        's' -> let sub:cmd = det in return $
            case sub of
                'd' ->
                    let newIdx = case cmd of
                            "n" -> (dIdx + 1) `rem` length dList
                            "p" -> (dIdx + length dList - 1) `rem` length dList
                            'i':num -> read num - 1
                            _ -> dIdx
                        resIdx = if newIdx >= length dList then dIdx else newIdx
                        newWState = Workspace wName resIdx dList
                        newWList = take wIdx wList ++ newWState : drop (wIdx + 1) wList
                    in UserState wIdx newWList
                'w' ->
                    let newIdx = case cmd of
                            "n" -> (wIdx + 1) `rem` length wList
                            "p" -> (wIdx + length wList - 1) `rem` length wList
                            'i':num -> read num - 1
                            _ -> wIdx
                        resIdx = if newIdx >= length wList then wIdx else newIdx
                    in UserState resIdx wList
                _ -> cState
        't' -> do
            let list = concatMap (\(Workspace wName _ dList) -> map (getDesktopName wName) dList) wList
            (Just stdin, Just stdout, _, _) <- createProcess $
                (shell "dmenu") { std_in = CreatePipe, std_out = CreatePipe }
            forM_ list $ hPutStrLn stdin
            hClose stdin
            (wspace, desktop) <- liftM getDesktopRepr $ hGetLine stdout
            let newWIdx = findIndex (\(Workspace name _ _) -> name == wspace) wList
            return $ case newWIdx of
                Nothing -> cState
                Just wIdx ->
                    let Workspace wName dIdx dList = wList !! wIdx
                        newDIdx = fromMaybe dIdx $ elemIndex desktop dList
                        newWState = Workspace wName newDIdx dList
                        newWList = take wIdx wList ++ newWState : drop (wIdx + 1) wList
                    in UserState wIdx newWList
        'a' -> do
            let newState = case det of
                    "d" ->
                        let newDList = dList ++ [show $ length dList + 1]
                            newWState = Workspace wName (length dList) newDList
                            newWList = take wIdx wList ++ newWState : drop (wIdx + 1) wList
                        in UserState wIdx newWList
                    "w" ->
                        let newWList = wList ++ [Workspace (show $ length wList + 1) 0 ["1"]]
                        in UserState (length wList) newWList
            runCommand $ "bspc monitor -a " ++ getWorkspaceName newState
            return newState
    runCommand $ "bspc desktop -f " ++ getWorkspaceName newState
    loop newState

main = loop $ UserState 0 [ Workspace "asm" 1 ["manual", "browser", "vim"]
                          , Workspace "itmos" 1 ["browser", "vim"]]
