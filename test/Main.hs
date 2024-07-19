{-# LANGUAGE TypeApplications, DataKinds #-}

module Main where

import Graphics.UI.Qml
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM

type St = Int
data Event = Incr | Decr | Nop

viewController :: St -> QViewModel Event
viewController s = rootObject "hs" $ do
  qProperty "counter" s
  qSlot "increment" (CType @'[])
    (return Incr)
  qSlot "decrement" (CType @'[])
    (return Decr)


update :: Event -> Qml St ()
update Incr = modify (\x -> x + 1)
update Decr = modify (\x -> x - 1)
update Nop  = return ()

main :: IO ()
main = do
  eChan <- newTChanIO
  forkIO $ fix $ \loop -> do
    threadDelay 1000000
    atomically $ writeTChan eChan Incr
    loop

  let app = QmlApp
        { qmlFile = "test/main.qml"
        , appUpdate = update
        , appViewModel = viewController
        , externalEvents = Just eChan
        }
  runQApplication app 0
