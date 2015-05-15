import Control.Monad

import Data.List

import Network.Socket

import System.Environment

{- A simple command utility. Human-readable format to be added. -}
main = do
    let socketPath = "/tmp/tompebar.socket"
    cmdSocket <- socket AF_UNIX Stream defaultProtocol
    connect cmdSocket $ SockAddrUnix socketPath
    liftM concat getArgs >>= send cmdSocket
    close cmdSocket
