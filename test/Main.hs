module Main where

import Graphics.UI.Qml.LowLevel.QApplication
import Graphics.UI.Qml.LowLevel.QQmlApplicationEngine

main = do
    app <- initQApplication
    ctx <- newQmlAppEngine

    setContextProperty ctx "qVar1" (10 :: Int)
    setContextProperty ctx "qVar2" "Hello World"
    setContextProperty ctx "qVar3" False
    loadQml ctx "test/main.qml"

    execQApplication app (Just ctx)