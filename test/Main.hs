{-# LANGUAGE TypeApplications, DataKinds #-}

module Main where

import Graphics.UI.Qml
import System.IO

type St = Int
data Event = Incr | Decr | Nop

viewController :: St -> QViewModel Event
viewController s = rootObject "haskell" $ do
  -- qSlot "increment" (CType @'[])
  --   (return Incr)
  -- qSlot "decrement" (CType @'[])
  --   (return Decr)
  qSlot "hello"
    (CType @'[])
    (putStrLn "Hello" >> return Nop)
  qSlot "numberTest"
    (CType @'[Int])
    (\n -> print n >> return Nop)
  qSlot "nameage" 
    (CType @[String,Int]) 
    (\name age -> do
        putStrLn $ "Hello " <> name <> " who is " <> show age
        return Incr)
  qProperty "counter" s


update :: Event -> Qml St ()
update Incr = modify (1 +)
update Decr = modify (1 -)
update Nop  = return ()

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let app = QmlApp
        { qmlFile = "test/main.qml"
        , appUpdate = update
        , appViewModel = viewController
        }
  runQApplication app 0
