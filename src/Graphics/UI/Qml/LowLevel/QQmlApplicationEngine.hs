
module Graphics.UI.Qml.LowLevel.QQmlApplicationEngine where

import qualified Graphics.UI.Qml.Internal.QQmlApplicationEngine as Raw
import qualified Graphics.UI.Qml.Internal.QQmlContext as Raw
import qualified Graphics.UI.Qml.Internal.Types as Raw

import qualified Data.ByteString as BS
import qualified Foreign.Concurrent as F
import Graphics.UI.Qml.LowLevel.QApplication
import Graphics.UI.Qml.LowLevel.QVariant
import Graphics.UI.Qml.LowLevel.QUrl

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C

type QmlAppEngine = Ptr Raw.DosQQmlApplicationEngine

foreign import ccall unsafe "string.h strndup"
    strndup :: CString -> CSize -> IO CString

class IsQApplication a where
  addFinalizerToApp :: a -> IO () -> IO ()

instance IsQApplication QApplication where
  addFinalizerToApp (QApplication app) = F.addForeignPtrFinalizer app

newQmlAppEngine :: (IsQApplication app) => app -> IO QmlAppEngine
newQmlAppEngine app = do
    ptr <- Raw.create
    addFinalizerToApp app (Raw.delete ptr)
    return ptr

loadQmlRaw :: QmlAppEngine -> BS.ByteString -> IO ()
loadQmlRaw ae file =
    BS.useAsCStringLen file $ \(f, len) ->
        strndup f (fromIntegral len) >>= Raw.loadData ae

loadQml :: QmlAppEngine -> FilePath -> IO ()
loadQml ae path = BS.readFile path >>= loadQmlRaw ae

addImportPath :: QmlAppEngine -> FilePath -> IO ()
addImportPath ctx path = withCString path $ Raw.addImportPath ctx

loadResource :: QmlAppEngine -> String -> IO ()
loadResource ctx url = do
  url' <- stringToQUrl url
  withForeignPtr url' $ Raw.loadUrl ctx

setContextProperty :: (IsQVariant a) => QmlAppEngine -> String -> a -> IO ()
setContextProperty eng key val = do
    ctx <- Raw.context eng
    var <- toQVariant val
    withCString key $ \propName ->
        withForeignPtr var $ \rawVar ->
        Raw.setContextProperty ctx propName rawVar
