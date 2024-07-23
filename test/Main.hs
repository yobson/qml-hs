{-# LANGUAGE TypeApplications, DataKinds, DeriveGeneric #-}

module Main where

import Data.Aeson
import Graphics.UI.Qml
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM
import GHC.Generics

-- We define our state type
type State = (Int, String)

-- We define our possible events
data Event = Incr | Decr | Welcome String Int | Nop

data TestObj = Test
  { prop1 :: Int
  , prop2 :: String
  } deriving (Generic, Eq)

instance ToJSON TestObj
instance FromJSON TestObj

-- We define a function that describes all of our slots and properties
viewController :: State -> QViewModel Event
viewController (count, msg) = rootObject "hs" $ do
  qSlot "increment" (CType @'[])
    (return Incr)
  qSlot "decrement" (CType @'[])
    (return Decr)
  qSlot "welcome" (CType @[String, Int])
    (\name age -> return $ Welcome name age)

  properties $ do
    qProperty "counter" count
    qProperty "welcomeMsg" msg

    -- Lists are fine! You can use repeaters
    qProperty @[Int] "fun" [1..3]

    -- Here we have a nested QObject. No slots are allowed in nested objects
    qProperty "Foo" $
      qProperty @Int "Bar" 42
      
    -- If you want to have lists of objects, you can use JsonData. Be sure to parse it from QML
    qProperty "obj" $ JsonData $ [ Test n "Hello" | n <- [0..count]]

-- We have a function that gets events from slots (and your custom channel if you like)
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
