{-# LANGUAGE NamedFieldPuns #-}

module VirtualNetwork where

sendAddr :: Network -> Int -> Int -> String -> IO ()
sendAddr net origin dest msg = do
	destDevice <- findDevice net dest
	let event = Receive { from = origin, msg = msg }
	eventHandler destDevice $ event

send :: Network -> Device -> Device -> String -> IO ()
send net origin dest msg = do
	sendAddr net (addr origin) (addr dest) msg

findDevice :: Network -> Int -> IO Device
findDevice net addrv = do
	let conns = connections net
	let dvces = filter (\dv -> addrv == addr dv) $ devices net
	case dvces of
		[] -> error $ "No device with address " ++ show addrv ++ " found"
		(x:xs) -> return x


createEventHandler :: ReceiveEventHandler -> EventHandler
createEventHandler receiveHandler = \event -> do
	case event of
		Receive { from, msg } -> do
			receiveHandler from msg
	


type ReceiveEventHandler = Int -> String -> IO ()
type EventHandler = Event -> IO ()

data DeviceConnection = DeviceConnection { dva :: Device, dvb :: Device }
data Connection = Connection { a :: Int, b :: Int }
data Device = Device { addr :: Int, eventHandler :: EventHandler }
data Event = Receive { from :: Int, msg :: String } deriving (Show)
data Network = Network { connections :: [Connection], devices :: [Device] }


emptyEventHandler :: EventHandler
emptyEventHandler = \event -> do return ()

newDevice :: Int -> EventHandler -> Device
newDevice addr eventHandler = Device { addr, eventHandler }

newNetworkRaw :: [Connection] -> [Device] -> Network
newNetworkRaw conns devices = Network { connections = conns, devices = devices }

newNetwork :: [DeviceConnection] -> Network
newNetwork dvcns = newNetworkRaw (map (\dvcn -> connectionRaw (addr $ dva dvcn) (addr $ dvb dvcn)) dvcns) (dvcnToDvc dvcns)

dvcnToDvc :: [DeviceConnection] -> [Device]
-- get all devices. no duplicates by comparing addr
dvcnToDvc dvcns = foldl (\acc dvcn -> if (addr $ dva dvcn) `elem` (map addr acc) then acc else (dva dvcn):acc) [] dvcns

connection :: Device -> Device -> DeviceConnection
connection a b = DeviceConnection { dva = a, dvb = b }

connectionRaw :: Int -> Int -> Connection
connectionRaw a b = Connection { a, b }

newDenseNetwork :: [Device] -> Network
newDenseNetwork devices = newNetworkRaw conns devices
	where
		conns = concat $ map (\(a, b) -> [Connection { a = a, b = b }, Connection { a = b, b = a }]) $ pairs $ map addr devices

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs
