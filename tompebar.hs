import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

import Data.Char
import Data.Maybe
import Data.List

import Network.Socket

import System.Directory
import System.Environment
import System.IO
import System.Process

{----------------- Required data structures: -----------------}

-- Workspace, consists of desktops.
data Workspace = Workspace { wName :: String
                           , dIdx :: Int
                           , dList :: [Desktop] }
-- A single desktop. We also need to know whether it contains windows.
data Desktop = Desktop { isOccupied :: Bool
                       , dName :: String }
-- The whole user state: a list of workspaces and index of the current one.
data UserState = UserState { wIdx :: Int
                           , wList :: [Workspace] }
-- bspc will give info about the desktops in this format.
data BspcEntry = BspcEntry { occupied :: Bool
                           , focused :: Bool
                           , workspace :: String
                           , desktop :: String }

{-------------------------------------------------------------}

separator = '$'

-- Get bspc representation of a desktop by the workspace and desktop names.
getDesktopName :: String -> String -> String
getDesktopName wsp dsk = wsp ++ separator : dsk

-- Get my representation of a desktop from the bspc one.
getDesktopRepr :: String -> (String, String)
getDesktopRepr str = let (workspace, desktop) = span (/= separator) str in (workspace, tail desktop)

-- Get current desktop name from a state.
getWorkspaceName :: UserState -> String
getWorkspaceName state = getDesktopName wname dname
    where
        cWorkspace = wList state !! wIdx state
        wname = wName cWorkspace
        dname = dName $ dList cWorkspace !! dIdx cWorkspace

-- Parse the output of "bspc control --subscribe".
parseInput :: String -> [BspcEntry]
parseInput = parseInput' []
  where
      parseInput' result (':':c:name)
          | toLower c `elem` "of" =
              let (desktop, cont) = span (/= ':') name
                  (w, d) = getDesktopRepr desktop
              in parseInput' (BspcEntry (toLower c == 'o') (isUpper c) w d : result) cont
          | otherwise = parseInput' result name
      parseInput' result (_:s) = parseInput' result s
      parseInput' result [] = result

-- Bspc may accidentially alter the current desktop - it should notify me about that.
subscribeBspc :: MVar UserState -> IO ()
subscribeBspc stateVar = do
    dState <- liftM parseInput getLine
    freeState <- tryTakeMVar stateVar -- If no luck, there could be readCommands renaming a
                                      -- workspace using several queries. Staying silent.
    when (isJust freeState) $ do
        -- Update the current user state.
        let Just cState = freeState
            BspcEntry _ _ wrName dtName = dState !! (fromJust $ findIndex focused dState)
            adjustDst dst =
                let bspcEntry = dState !! (fromJust $ findIndex ((== dName dst) . desktop) dState)
                in dst { isOccupied = occupied bspcEntry }
            adjustWsp workspace = workspace { dList = map adjustDst $ dList workspace }
            occAdjust = map adjustWsp $ wList cState
            Just wIndex = findIndex ((== wrName) . wName) occAdjust
            Just dIndex = findIndex ((== dtName) . dName) (dList $ occAdjust !! wIndex)
            newWState = (occAdjust !! wIndex) { dIdx = dIndex }
            newState = UserState wIndex $ (occAdjust `modInd` wIndex) newWState
        formatBar newState
        putMVar stateVar newState
    subscribeBspc stateVar

modInd :: [a] -> Int -> a -> [a]
modInd list idx newElem = let (pref, suff) = splitAt idx list in pref ++ newElem : tail suff

bspc :: [String] -> IO ()
bspc = callProcess "bspc"

-- Format the current user state to output it into bar or somewhere.
-- TODO
--  * Add format customization
--  * Try to use coloring tools of bar-aint-recursive
formatBar :: UserState -> IO ()
formatBar (UserState wIdx wList) = do
    getArgs >>= (`forM_` putStr)
    let (pref, Workspace cName dIdx dList : suff) = splitAt wIdx wList
    putChar '|'
    forM_ pref $ \(Workspace name _ _) -> putStr $ ' ':name ++ " |"
    putStr $ ' ':cName ++ " {"
    forM_ (zip [0..] dList) $ \(num, name) -> do
        putChar ' '
        when (num == dIdx) $ putChar '['
        putStr $ dName name
        when (num == dIdx) $ putChar ']'
    putStr " } |"
    forM_ suff $ \(Workspace name _ _) -> putStr $ ' ':name ++ " |"
    putStrLn ""
    hFlush stdout

data BufferedInput = BufferedInput Socket [String]

-- Accept a client (tbctl) and read its command(s).
getCommand :: BufferedInput -> IO (String, BufferedInput)
getCommand (BufferedInput sock list) = do
    let awaitForClient = do
            (client, _) <- accept sock
            cHandle <- socketToHandle client ReadMode
            hSetBuffering cHandle LineBuffering
            result <- liftM lines $ hGetContents cHandle
            if null result
                then awaitForClient
                else return result
    cmd:rem <- if null list
        then awaitForClient
        else return list
    return (cmd, BufferedInput sock rem)

-- Process user commands.
readCommands :: MVar UserState -> BufferedInput -> IO ()
readCommands stateVar buffer = do
    (cmd:det, buffer) <- getCommand buffer
    cState <- takeMVar stateVar
    newState <- case cmd of
        's' -> return $ -- s[d|w][p|n|number] - switch desktop/workspace to a prev/next/specified one.
            case det of
                'd':cmd -> let cWorkspace = wList cState !! wIdx cState
                               dIndex = dIdx cWorkspace
                               nDesktops = length $ dList cWorkspace
                               nIndex = case cmd of
                                   "p" -> (dIndex + nDesktops - 1) `rem` nDesktops
                                   "n" -> (dIndex + 1) `rem` nDesktops
                                   _   -> read cmd - 1
                               fIndex = if nIndex >= nDesktops || nIndex < 0
                                           then dIndex else nIndex
                               nWorkspace = cWorkspace { dIdx = fIndex }
                           in cState { wList = (wList cState `modInd` wIdx cState) nWorkspace }
                'w':cmd -> let nWorkspaces = length $ wList cState
                               nIndex = case cmd of
                                   "p" -> (wIdx cState + nWorkspaces - 1) `rem` nWorkspaces
                                   "n" -> (wIdx cState + 1) `rem` nWorkspaces
                                   _   -> read cmd - 1
                               fIndex = if nIndex >= nWorkspaces || nIndex < 0
                                           then wIdx cState else nIndex
                           in cState { wIdx = fIndex }
                _ -> cState
        'a' -> do -- a[d|w]name - add a desktop/workspace with a given name
            let result = case det of
                    'd':name -> let cWorkspace = wList cState !! wIdx cState
                                    ndIdx = length $ dList cWorkspace
                                    ndList = dList cWorkspace ++ [Desktop False name]
                                    nWorkspace = cWorkspace { dIdx = ndIdx, dList = ndList }
                                    exists = findIndex ((== name) . dName) $ dList cWorkspace
                                    nwList = (wList cState `modInd` wIdx cState) nWorkspace
                                in guard (isNothing exists) >> Just (cState { wList = nwList })
                    'w':name -> let nWorkspace = Workspace name 0 [Desktop False "browser"]
                                    nwList = wList cState ++ [nWorkspace]
                                    exists = findIndex ((== name) . wName) $ wList cState
                                    nwIdx = length $ wList cState
                                in guard (isNothing exists) >> Just (UserState nwIdx nwList)
                    _        -> Nothing
            case result of
                Nothing -> return cState
                Just nState -> bspc ["monitor", "-a", getWorkspaceName nState] >> return nState
        'r' -> case det of -- r[d|w]name - rename a desktop/workspace
            'd':name -> let cWorkspace = wList cState !! wIdx cState
                            nDesktop = (dList cWorkspace !! dIdx cWorkspace) { dName = name }
                            ndList = (dList cWorkspace `modInd` dIdx cWorkspace) nDesktop
                            nWorkspace = cWorkspace { dList = ndList }
                            nwList = (wList cState `modInd` wIdx cState) nWorkspace
                            exists = findIndex ((== name) . dName) $ dList cWorkspace
                        in if isNothing exists
                            then do let result = cState { wList = nwList }
                                    bspc ["desktop", "-n", getWorkspaceName result]
                                    return result
                            else return cState
            'w':name -> let cWorkspace = wList cState !! wIdx cState
                            exists = findIndex ((== name) . wName) $ wList cState
                            nWorkspace = cWorkspace { wName = name }
                            nwList = (wList cState `modInd` wIdx cState) nWorkspace
                        in if isNothing exists
                            then do let result = cState { wList = nwList }
                                        oldName = getDesktopName (wName cWorkspace)
                                        newName = getDesktopName name
                                    forM (map dName $ dList nWorkspace) $ \dName ->
                                        bspc ["desktop", oldName dName, "-n", newName dName]
                                    return result
                            else return cState
            _        -> return cState
        'd' -> -- d - remove a free desktop
            let cWorkspace = wList cState !! wIdx cState
                checkOccupied = isOccupied $ dList cWorkspace !! dIdx cWorkspace
                checkLastDesktop = length (wList cState) == 1 && length (dList cWorkspace) == 1
            in if checkOccupied || checkLastDesktop then return cState
                else do
                    let result = if length (dList cWorkspace) > 1
                            then let (p, s) = splitAt (dIdx cWorkspace) (dList cWorkspace)
                                     ndList = p ++ tail s
                                     ndIdx = min (length ndList - 1) $ dIdx cWorkspace
                                     nWspc = cWorkspace { dIdx = ndIdx, dList = ndList }
                                 in cState { wList = (wList cState `modInd` wIdx cState) nWspc }
                            else let (p, s) = splitAt (wIdx cState) (wList cState)
                                     nwList = p ++ tail s
                                     nwIdx = min (length nwList - 1) $ wIdx cState
                                 in UserState nwIdx nwList
                    bspc ["desktop", "-f", getWorkspaceName result]
                    bspc ["monitor", "-r", getWorkspaceName cState]
                    return result
        _   -> return cState
    bspc ["desktop", "-f", getWorkspaceName newState]
    formatBar newState
    putMVar stateVar newState
    readCommands stateVar buffer

main = do
    let socketPath = "/tmp/tompebar.socket"
    doesFileExist socketPath >>= (`when` removeFile socketPath)
    inputSocket <- socket AF_UNIX Stream defaultProtocol
    bind inputSocket $ SockAddrUnix socketPath
    listen inputSocket 1
    -- We should first obtain our initial state.
    initList <- liftM parseInput getLine
    let addDesktop cState entry = case findIndex ((== workspace entry) . wName) $ wList cState of
            Nothing -> let nDesktop = Desktop (occupied entry) (desktop entry)
                           nWorkspace = Workspace (workspace entry) 0 [nDesktop]
                           nwIdx = if focused entry then length $ wList cState else wIdx cState
                       in UserState nwIdx (wList cState ++ [nWorkspace])
            Just number -> let cWorkspace = wList cState !! number
                               nDesktop = Desktop (occupied entry) (desktop entry)
                               ndIdx = if focused entry
                                   then length $ dList cWorkspace else dIdx cWorkspace
                               ndList = dList cWorkspace ++ [nDesktop]
                               nWorkspace = cWorkspace { dIdx = ndIdx, dList = ndList }
                               nwIdx = if focused entry then length $ wList cState else wIdx cState
                           in UserState nwIdx ((wList cState `modInd` number) nWorkspace)
        initState = foldl addDesktop (UserState undefined []) initList
    formatBar initState
    uState <- newMVar initState
    forkIO $ subscribeBspc uState
    readCommands uState $ BufferedInput inputSocket []
