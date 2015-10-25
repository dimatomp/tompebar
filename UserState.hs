module UserState where

import Control.Monad

import Data.List
import Data.Maybe

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

-- Get current desktop name from a state.
getWorkspaceName :: UserState -> String
getWorkspaceName state = getDesktopName wname dname
    where
        cWorkspace = wList state !! wIdx state
        wname = wName cWorkspace
        dname = dName $ dList cWorkspace !! dIdx cWorkspace

separator = '$'

-- Get bspc representation of a desktop from the workspace and desktop names.
getDesktopName :: String -> String -> String
getDesktopName "<workspace>" dsk = dsk
getDesktopName wsp           dsk = wsp ++ separator : dsk

-- Get my representation of a desktop from the bspc one.
getDesktopRepr :: String -> (String, String)
getDesktopRepr str =
    let (workspace, desktop) = span (/= separator) str
    in if not $ null desktop then (workspace, tail desktop) else ("<workspace>", workspace)

modInd :: Int -> a -> [a] -> [a]
modInd idx newElem list = let (pref, suff) = splitAt idx list in pref ++ newElem : tail suff

infixl 1 &
(&) = flip ($)

switchDesktop :: UserState -> (Int -> Int -> Int) -> UserState
switchDesktop (UserState wIdx wList) func  =
    let cWorkspace = wList !! wIdx
        dIndex = dIdx cWorkspace
        nDesktops = length $ dList cWorkspace
        nIndex = func dIndex nDesktops
        resultIndex = if nIndex >= nDesktops || nIndex < 0 then dIndex else nIndex
        nWorkspace = cWorkspace {dIdx = resultIndex}
    in UserState wIdx $ wList & modInd wIdx nWorkspace

switchWorkspace :: UserState -> (Int -> Int -> Int) -> UserState
switchWorkspace (UserState wIdx wList) func =
    let nWorkspaces = length wList
        nIndex = func wIdx nWorkspaces
        resultIndex = if nIndex >= nWorkspaces || nIndex < 0 then wIdx else nIndex
    in UserState resultIndex wList

addDesktop :: Monad m => String -> UserState -> (String -> m ()) -> m UserState
addDesktop name cState@(UserState wIdx wList) apply =
    let cWorkspace = wList !! wIdx
        ndIdx = length $ dList cWorkspace
        ndList = dList cWorkspace ++ [Desktop False name]
        nWorkspace = cWorkspace { dIdx = ndIdx, dList = ndList }
        doesNotExist = isNothing $ findIndex ((== name) . dName) $ dList cWorkspace
        result = UserState wIdx $ wList & modInd wIdx nWorkspace
    in if doesNotExist
        then result <$ apply (getWorkspaceName result)
        else return cState

addWorkspace :: Monad m => String -> UserState -> (String -> m ()) -> m UserState
addWorkspace name cState@(UserState wIdx wList) apply =
    let nWorkspace = Workspace name 0 [Desktop False "browser"]
        doesNotExist = isNothing $ findIndex ((== name) . wName) wList
        result = UserState (length wList) (wList ++ [nWorkspace])
    in if doesNotExist
        then result <$ apply (getWorkspaceName result)
        else return cState

renameDesktop :: Monad m => String -> UserState -> (String -> m ()) -> m UserState
renameDesktop name cState@(UserState wIdx wList) apply =
    let cWorkspace = wList !! wIdx
        nDesktop = (dList cWorkspace !! dIdx cWorkspace) { dName = name }
        ndList = dList cWorkspace & modInd (dIdx cWorkspace) nDesktop
        nWorkspace = cWorkspace { dList = ndList }
        doesNotExist = isNothing $ findIndex ((== name) . dName) $ dList cWorkspace
        result = UserState wIdx $ wList & modInd wIdx nWorkspace
    in if doesNotExist
        then result <$ apply (getWorkspaceName result)
        else return cState

renameWorkspace :: Monad m => String -> UserState -> (String -> String -> m ()) -> m UserState
renameWorkspace name cState@(UserState wIdx wList) apply =
    let cWorkspace = wList !! wIdx
        doesNotExist = isNothing $ findIndex ((== name) . wName) $ wList
        nWorkspace = cWorkspace { wName = name }
        result = UserState wIdx $ wList & modInd wIdx nWorkspace
        oldName = getDesktopName (wName cWorkspace)
        newName = getDesktopName name
    in if doesNotExist
        then result <$ forM_ (map dName $ dList nWorkspace) (liftM2 apply oldName newName)
        else return cState

removeDesktop :: Monad m => UserState -> (String -> String -> m ()) -> m UserState
removeDesktop cState@(UserState wIdx wList) apply =
    let cWorkspace = wList !! wIdx
        checkOccupied = isOccupied $ dList cWorkspace !! dIdx cWorkspace
        checkLastDesktop = length wList == 1 && length (dList cWorkspace) == 1
        result = if length (dList cWorkspace) > 1
            then let (p, s) = splitAt (dIdx cWorkspace) (dList cWorkspace)
                     ndList = p ++ tail s
                     ndIdx = min (length ndList - 1) $ dIdx cWorkspace
                     nWspc = cWorkspace { dIdx = ndIdx, dList = ndList }
                 in UserState wIdx $ wList & modInd wIdx nWspc
            else let (p, s) = splitAt wIdx wList
                     nwList = p ++ tail s
                     nwIdx = min (length nwList - 1) wIdx
                 in UserState nwIdx nwList
    in if checkOccupied || checkLastDesktop
        then return cState
        else result <$ apply (getWorkspaceName cState) (getWorkspaceName result)

addBspcEntry :: UserState -> BspcEntry -> UserState
addBspcEntry (UserState wIdx wList) entry = case findIndex ((== workspace entry) . wName) wList of
    Nothing ->
        let nDesktop = Desktop (occupied entry) (desktop entry)
            nWorkspace = Workspace (workspace entry) 0 [nDesktop]
            nwIdx = if focused entry then length wList else wIdx
        in UserState nwIdx (wList ++ [nWorkspace])
    Just number ->
        let cWorkspace = wList !! number
            nDesktop = Desktop (occupied entry) (desktop entry)
            ndIdx = if focused entry
                then length $ dList cWorkspace else dIdx cWorkspace
            ndList = dList cWorkspace ++ [nDesktop]
            nWorkspace = cWorkspace { dIdx = ndIdx, dList = ndList }
            nwIdx = if focused entry then number else wIdx
        in UserState nwIdx (wList & modInd number nWorkspace)
