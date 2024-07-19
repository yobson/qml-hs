{-# LANGUAGE TypeApplications, DataKinds #-}

module Main where

import Graphics.UI.Qml
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM

type State = (Int, String)
data Event = Incr | Decr | Welcome String Int | Nop

viewController :: State -> QViewModel Event
viewController (count, msg) = rootObject "hs" $ do
  qProperty "counter" count
  qProperty "welcomeMsg" msg
  qSlot "increment" (CType @'[])
    (return Incr)
  qSlot "decrement" (CType @'[])
    (return Decr)
  qSlot "welcome" (CType @[String, Int])
    (\name age -> return $ Welcome name age)


update :: Event -> Qml State ()
update Incr               = modify (\(x,m) -> (x + 1,m))
update Decr               = modify (\(x,m) -> (x - 1,m))
update (Welcome name age) = modify (\(x,_) -> (x,"Welcome " <> name <> " who is " <> show age))
update Nop                = return ()

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
  runQApplication app (0, "Fill out details")
