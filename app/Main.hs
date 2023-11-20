{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad.State
import System.Random
import VirtualNetwork

main :: IO ()
main = do
	let device1EventHandler = createEventHandler \from msg ->
		putStrLn $ "Received message from " ++ show from ++ ": " ++ msg

	let device1 = newDevice 1 device1EventHandler
	let device2 = newDevice 2 emptyEventHandler

	let network = newNetwork [connection device1 device2]
	send network device2 device1 "Hello, World!"

