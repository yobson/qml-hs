
module Graphics.UI.Qml.LowLevel.QQmlApplicationEngine where

import qualified Graphics.UI.Qml.Internal.QQmlApplicationEngine as Raw
import qualified Graphics.UI.Qml.Internal.QQmlContext as Raw
import qualified Graphics.UI.Qml.Internal.Types as Raw

import qualified Data.ByteString as BS
import Graphics.UI.Qml.LowLevel.QVariant

import Foreign.ForeignPtr
import Foreign.C

type QmlAppEngine = ForeignPtr Raw.DosQQmlApplicationEngine

foreign import ccall unsafe "string.h strndup"
    strndup :: CString -> CSize -> IO CString

newQmlAppEngine :: IO QmlAppEngine
newQmlAppEngine = do
    ptr <- Raw.create
    newForeignPtr Raw.finialiser ptr

loadQmlRaw :: QmlAppEngine -> BS.ByteString -> IO ()
loadQmlRaw ae file = withForeignPtr ae $ \ptr ->
    BS.useAsCStringLen file $ \(f, len) ->
        strndup f (fromIntegral len) >>= Raw.loadData ptr

loadQml :: QmlAppEngine -> FilePath -> IO ()
loadQml ae path = BS.readFile path >>= loadQmlRaw ae

setContextProperty :: (IsQVariant a) => QmlAppEngine -> String -> a -> IO ()
setContextProperty eng key val = withForeignPtr eng $ \ptr -> do
    ctx <- Raw.context ptr
    var <- toQVarient val
    withCString key $ \propName ->
        withForeignPtr var $ \rawVar ->
        Raw.setContextProperty ctx propName rawVar