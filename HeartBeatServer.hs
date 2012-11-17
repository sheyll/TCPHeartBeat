import Network
import System.Environment
import System.Time
import System.IO
import Control.Concurrent

main = do
	args <- getArgs
	pName <- getProgName
	case args of
		[portStr] -> startServer (read portStr)
		_ -> putStrLn ("Starts a TCP heart beat server on the given ip and port.\nUsage: "++pName++" <port>")

startServer :: Integer -> IO ()
startServer port = do
	sock <- listenOn (PortNumber $ fromInteger port)
	logger $ " Server Started on port " ++ (show port)
	serverLoop sock 0

serverLoop sock ci = do
	a@(h, host, port) <- accept sock
	logger $  "Client " ++ (show ci) ++ " connected: " ++ (show host) ++ " port " ++ (show port)
	hSetBuffering h LineBuffering
	forkIO (clientLoop a (show ci))
	serverLoop sock (ci + 1)
		
clientLoop (h, host, port) ci = _loop
    where _loop = do
		logger $ "client " ++ ci ++ " sending ping...:"
		hPutStrLn h "I CAN HAZ CHEEZBURGER???!!!11"
		ca <- hGetLine h
		logger $ "client " ++ ci ++ " answered: " ++ ca
		threadDelay 3000000
		_loop

printTime = getClockTime >>= putStr . show

logger x = printTime >> (putStrLn $ " - " ++ x)
