import Control.Monad

import Data.List

import Network.Socket

import System.Environment

{- A simple command utility. -}
main = do
    let socketPath = "/tmp/tompebar.socket"
    command <- getArgs
    let getDir "prev" = "p"
        getDir "next" = "n"
        getDir num    = show $ (read num :: Int)
        translated = case command of
            "--add-desktop":name:_ -> "ad" ++ name
            "--add-workspace":name:_ -> "aw" ++ name
            "--switch-desktop":dir:_ -> "sd" ++ getDir dir
            "--switch-workspace":dir:_ -> "sw" ++ getDir dir
            "--rename-desktop":name:_ -> "rd" ++ name
            "--rename-workspace":name:_ -> "rw" ++ name
            "--remove-desktop":_ -> "d"
            _ -> ""
    case translated of
        "" -> do name <- liftM ("    " ++) getProgName
                 putStrLn $ "tompebar control utility. Usage: "
                 putStrLn $ name ++ " --add-[desktop|workspace] <name>"
                 putStrLn $ name ++ " --switch-[desktop|workspace] [prev|next|<number>]"
                 putStrLn $ name ++ " --rename-[desktop|workspace] <name>"
                 putStrLn $ name ++ " --remove-desktop"
        _ -> do
            cmdSocket <- socket AF_UNIX Stream defaultProtocol
            connect cmdSocket $ SockAddrUnix socketPath
            send cmdSocket translated
            close cmdSocket
