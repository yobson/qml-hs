
module Graphics.UI.Qml.LowLevel.QApplication where

import qualified Graphics.UI.Qml.Internal.QApplication as Raw
import qualified Foreign.Concurrent as F
import Foreign.ForeignPtr
import Foreign.Ptr

newtype QApplication = QApplication (ForeignPtr ())

initQApplication :: IO QApplication
initQApplication = do
    Raw.create
    QApplication <$> F.newForeignPtr nullPtr Raw.delete

execQApplication :: QApplication -> IO ()
execQApplication (QApplication app) = withForeignPtr app $ const Raw.exec
