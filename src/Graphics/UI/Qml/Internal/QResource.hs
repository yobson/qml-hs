
module Graphics.UI.Qml.Internal.QResource where

import Foreign.C

foreign import ccall "dos_qresource_register"
    register :: CString -> IO ()