import Network.Socket

import System.Environment

main = do
    let socketPath = "/tmp/tompebar.socket"
    cmdSocket <- socket AF_UNIX Stream defaultProtocol
    connect cmdSocket $ SockAddrUnix socketPath
    let write = (>> return ()) . send cmdSocket
    (command:detail) <- getArgs
    case command of
        "switch" -> do
            write "s"
            let entity:[command] = detail
            write $ [head entity] ++ case command of
                "next" -> "n"
                "prev" -> "p"
                number -> 'i':number
        "choose" -> write "t"
        "add" -> write $ 'a' : (head $ head detail) : []
        "rename" -> do
            write "r"
            let entity:[command] = detail
            write $ [head entity] ++ command
        _ -> putStrLn "Invalid syntax"
    close cmdSocket
