
import Network.Socket (withSocketsDo, socket, Family( AF_UNIX ), SocketType( Stream ), defaultProtocol, bindSocket, SockAddr( SockAddrUnix ), listen)
import Network (accept, sClose)
import System.IO (hGetContents)

main = withSocketsDo $ do
    s <- socket AF_UNIX Stream defaultProtocol
    bindSocket s (SockAddrUnix "/tmp/misocketlindo.sock")
    listen s 5
    (h, hn, pn) <- accept s
    c <- hGetContents h
    sClose s
    putStrLn c