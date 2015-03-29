import Control.Monad

import Data.Char
import Data.Maybe
import Data.List

import Network.Socket

import System.Directory
import System.IO
import System.Process

data Workspace = Workspace String Int [String] deriving Show
data UserState = UserState Int [Workspace] deriving Show

separator = '$'

getDesktopName :: String -> String -> String
getDesktopName wsp dsk = wsp ++ separator : dsk

getDesktopRepr :: String -> (String, String)
getDesktopRepr str = let (workspace, desktop) = span (/= separator) str in (workspace, tail desktop)

getWorkspaceName :: UserState -> String
getWorkspaceName (UserState wIdx wList) =
    let Workspace wName dIdx dList = wList !! wIdx
    in getDesktopName wName (dList !! dIdx)

modInd :: [a] -> Int -> a -> [a]
modInd list idx newElem = let (pref, suff) = splitAt idx list in pref ++ newElem : tail suff

bspc :: [String] -> IO ()
bspc = callProcess "bspc"

readUserState :: IO UserState
readUserState = do
    let parseInput (':':c:name)
            | toLower c `elem` "of" =
                let (desktop, cont) = span (/= ':') name
                in (desktop, isUpper c) : parseInput cont
            | otherwise = parseInput name
        parseInput (_:s) = parseInput s
        parseInput [] = []
    cDesktops <- liftM parseInput $ readProcess "bspc" ["control", "--get-status"] ""
    let split = map (\(s, f) -> let (a, b) = span (/= separator) s in (a, tail b, f)) cDesktops
        sorted = sortBy (\(a, _, _) (b, _, _) -> a `compare` b) split
        grouped = groupBy (\(a, _, _) (b, _, _) -> a == b) $ sorted
        createWorkspace list@((a, _, _):s) =
            let cIndex = findIndex (\(_, _, b) -> b) list
            in (Workspace a (fromMaybe 0 cIndex) (map (\(_, d, _) -> d) list), isJust cIndex)
        resList = map createWorkspace grouped
    return $ UserState (fromJust $ findIndex snd resList) (map fst resList)

formatBar :: UserState -> IO ()
formatBar (UserState wIdx wList) = do
    let (pref, Workspace cName dIdx dList : suff) = splitAt wIdx wList
    putChar '|'
    forM_ pref $ \(Workspace name _ _) -> putStr $ ' ':name ++ " |"
    putStr $ ' ':cName ++ " {"
    forM_ (zip [0..] dList) $ \(num, name) -> do
        putChar ' '
        when (num == dIdx) $ putChar '['
        putStr name
        when (num == dIdx) $ putChar ']'
    putStr " } |"
    forM_ suff $ \(Workspace name _ _) -> putStr $ ' ':name ++ " |"
    putStrLn ""

data BufferedInput = BufferedInput Socket [String]

getCommand :: BufferedInput -> IO (String, BufferedInput)
getCommand (BufferedInput sock list) = do
    cmd:rem <- if null list
        then do
            (client, _) <- accept sock
            cHandle <- socketToHandle client ReadMode
            hSetBuffering cHandle LineBuffering
            liftM lines $ hGetContents cHandle
        else return list
    return (cmd, BufferedInput sock rem)

loop :: BufferedInput -> UserState -> IO ()
loop buffer cState@(UserState wIdx wList) = do
    formatBar cState
    (main:det, buffer) <- getCommand buffer
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
                        newWList = (wList `modInd` wIdx) newWState
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
                    in UserState wIdx $ (wList `modInd` wIdx) newWState
        'a' -> do
            let newState = case det of
                    "d" ->
                        let newDList = dList ++ [show $ length dList + 1]
                            newWState = Workspace wName (length dList) newDList
                        in UserState wIdx $ (wList `modInd` wIdx) newWState
                    "w" ->
                        let newWList = wList ++ [Workspace (show $ length wList + 1) 0 ["1"]]
                        in UserState (length wList) newWList
                    _ -> cState
            bspc ["monitor", "-a", getWorkspaceName newState]
            return newState
        'r' -> do
            let newState@(UserState _ nwList) = case det of
                    'd':name ->
                        let newDList = (dList `modInd` dIdx) name
                            newWState = Workspace wName dIdx newDList
                        in UserState wIdx $ (wList `modInd` wIdx) newWState
                    'w':name ->
                        let newWList = (wList `modInd` wIdx) $ Workspace name dIdx dList
                        in UserState wIdx newWList
                    _ -> cState
                Workspace nwName _ ndList = nwList !! wIdx
            forM_ (zip dList ndList) $ \(old, new) ->
                bspc ["desktop", getDesktopName wName old,
                     "-n", getDesktopName nwName new]
            bspc ["desktop", "-n", getWorkspaceName newState]
            return newState
        _ -> return cState
    bspc ["desktop", "-f", getWorkspaceName newState]
    loop buffer

main = do
    let socketPath = "/tmp/tompebar.socket"
    doesFileExist socketPath >>= (`when` removeFile socketPath)
    inputSocket <- socket AF_UNIX Stream defaultProtocol
    bind inputSocket $ SockAddrUnix socketPath
    listen inputSocket 1
    readUserState >>= loop (BufferedInput inputSocket [])
