
module Graphics.UI.Qml.LowLevel.QApplication where

import qualified Graphics.UI.Qml.Internal.QApplication as Raw
import qualified Foreign.Concurrent as F
import Foreign.ForeignPtr
import Foreign.Ptr

import Graphics.UI.Qml.LowLevel.QQmlApplicationEngine

newtype QApplication = QApplication (ForeignPtr ())

initQApplication :: IO QApplication
initQApplication = do
    Raw.create
    QApplication <$> F.newForeignPtr nullPtr Raw.delete

execQApplication :: QApplication -> Maybe QmlAppEngine -> IO ()
execQApplication (QApplication app) Nothing = withForeignPtr app $ const Raw.exec
execQApplication (QApplication app) (Just eng) = withForeignPtr eng $ const $ withForeignPtr app $ const Raw.exec
{-# NOINLINE execQApplication #-}
