
module Graphics.UI.Qml.LowLevel.QUrl where


import qualified Graphics.UI.Qml.Internal.QUrl as Raw
import qualified Graphics.UI.Qml.Internal.Types as Raw

import Foreign.C.String
import Foreign.ForeignPtr

type QUrl = ForeignPtr Raw.DosQUrl

stringToQUrl :: String -> IO QUrl
stringToQUrl url = withCString url $ \cstr -> do
  ptr <- Raw.create cstr 1
  newForeignPtr Raw.finalizer ptr
