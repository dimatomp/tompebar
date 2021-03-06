import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

import Data.Char
import Data.IORef
import Data.Maybe
import Data.List

import Network.Socket

import System.Directory
import System.Environment
import System.IO
import System.Process

import UserState

-- Parse the output of "bspc control --get-status".
parseInput :: String -> [BspcEntry]
parseInput = reverse . parseInput' []
  where
      parseInput' result (':':c:name)
          | toLower c `elem` "of" =
              let (desktop, cont) = span (/= ':') name
                  (w, d) = getDesktopRepr desktop
              in parseInput' (BspcEntry (toLower c == 'o') (isUpper c) w d : result) cont
          | otherwise = parseInput' result name
      parseInput' result (_:s) = parseInput' result s
      parseInput' result [] = result

-- bspwm may have accidentially switched the current desktop - it should notify me about that.
subscribeBspc :: MVar () -> IORef (UserState, Int) -> IO ()
subscribeBspc format stateVar = do
    dState <- liftM parseInput getLine
    newState <- atomicModifyIORef stateVar $ \(cState, count) -> if count == 0
        then let BspcEntry _ _ wrName dtName = dState !! (fromJust $ findIndex focused dState)
                 adjustDst wsp dst =
                     let bspcEntry = dState !! (fromJust $ findIndex (\entry -> workspace entry == wsp && desktop entry == dName dst) dState)
                     in dst { isOccupied = occupied bspcEntry }
                 adjustWsp workspace = workspace { dList = map (adjustDst $ wName workspace) $ dList workspace }
                 occAdjust = map adjustWsp $ wList cState
                 Just wIndex = findIndex ((== wrName) . wName) occAdjust
                 Just dIndex = findIndex ((== dtName) . dName) (dList $ occAdjust !! wIndex)
                 newWState = (occAdjust !! wIndex) { dIdx = dIndex }
                 newState = UserState wIndex $ occAdjust & modInd wIndex newWState
             in ((newState, count), Just newState)
        else ((cState, count - 1), Nothing)
    when (isJust newState) $ formatBar format $ fromJust newState
    subscribeBspc format stateVar


bspc :: [String] -> IO ()
bspc = callProcess "bspc"

-- Format the current user state to output it into bar or somewhere.
-- TODO
--  * Add format customization
--  * Try to use coloring tools of lemonbar
formatBar :: MVar () -> UserState -> IO ()
formatBar format (UserState wIdx wList) = do
    takeMVar format
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
    putMVar format ()

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
readCommands :: MVar () -> IORef (UserState, Int) -> BufferedInput -> IO ()
readCommands format stateVar buffer = do
    (cmd:det, buffer) <- getCommand buffer
    (cState, _) <- readIORef stateVar
    newState <- case cmd of
        's' -> return $ -- s[d|w][p|n|number] - switch desktop/workspace to a prev/next/specified one.
            case det of
                'd':cmd -> cState & switchDesktop $ \dIndex nDesktops -> case cmd of
                    "p" -> (dIndex + nDesktops - 1) `rem` nDesktops
                    "n" -> (dIndex + 1) `rem` nDesktops
                    _   -> read cmd - 1
                'w':cmd -> cState & switchWorkspace $ \wIdx nWorkspaces -> case cmd of
                    "p" -> (wIdx + nWorkspaces - 1) `rem` nWorkspaces
                    "n" -> (wIdx + 1) `rem` nWorkspaces
                    _   -> read cmd - 1
                _ -> cState
        'a' -> do -- a[d|w]name - add a desktop/workspace with a given name
            let command wName = bspc ["monitor", "-a", wName]
             in case det of
                 'd':name -> cState & addDesktop name $ command
                 'w':name -> cState & addWorkspace name $ command
                 _        -> return cState
        'r' -> case det of -- r[d|w]name - rename a desktop/workspace
            'd':name -> cState & renameDesktop name $ \wName -> bspc ["desktop", "-n", wName]
            'w':name -> cState & renameWorkspace name $ \old new -> do
                atomicModifyIORef stateVar $ \(cState, counter) -> ((cState, counter + 1), ())
                bspc ["desktop", old, "-n", new]
            _        -> return cState
        'd' -> -- d - remove a free desktop
            cState & removeDesktop $ \toDelete toSwitch -> do
                atomicModifyIORef stateVar $ \(cState, counter) -> ((cState, counter + 1), ())
                bspc ["desktop", "-f", toSwitch]
                bspc ["desktop", toDelete, "-r"]
        _   -> return cState
    bspc ["desktop", "-f", getWorkspaceName newState]
    formatBar format newState
    atomicModifyIORef stateVar $ \(_, counter) -> ((newState, counter), ())
    readCommands format stateVar buffer

main = do
    let socketPath = "/tmp/tompebar.socket"
    doesFileExist socketPath >>= (`when` removeFile socketPath)
    inputSocket <- socket AF_UNIX Stream defaultProtocol
    bind inputSocket $ SockAddrUnix socketPath
    listen inputSocket 1
    -- We should first obtain our initial state.
    initList <- liftM parseInput getLine
    let initState = foldl addBspcEntry (UserState undefined []) initList
    format <- newMVar ()
    formatBar format initState
    uState <- newIORef (initState, 0)
    forkIO $ subscribeBspc format uState
    readCommands format uState $ BufferedInput inputSocket []
