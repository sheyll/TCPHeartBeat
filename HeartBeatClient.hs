import Network
import System.Environment
import System.Time
import System.IO
import Control.Concurrent

main = do
	args <- getArgs
	pName <- getProgName
	case args of
		[hostStr, portStr, timeout] -> do
			h <- connectTo hostStr ((PortNumber . fromInteger . read) portStr)
			hSetBuffering h LineBuffering
			logger "Client connected."
			clientLoop h (read timeout)
		_ -> putStrLn ("Starts a TCP heart beat client, connected to the given host and port.\nUsage: "++pName++" <host> <port> <timeout>")


clientLoop :: Handle -> Int -> IO ()
clientLoop h t = do 
	logger "waiting for ping."
	dataAv <- hWaitForInput h t
	if dataAv then do
			l <- hGetLine h
			logger $ "got ping message: " ++ l
			hPutStrLn h "NOES!!!11"
			logger "sent answer"
			logger "==================================================================="
			clientLoop h t
	 else logger "Reading from server timed out!"

printTime = getClockTime >>= putStr . show

logger x = printTime >> (putStrLn $ " - " ++ x)
