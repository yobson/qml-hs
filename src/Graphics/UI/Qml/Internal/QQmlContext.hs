
module Graphics.UI.Qml.Internal.QQmlContext where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qqmlcontext_baseUrl"
    baseUrl :: Ptr DosQQmlContext -> IO CString

foreign import ccall "dos_qqmlcontext_setcontextproperty"
    setContextProperty :: Ptr DosQQmlContext -> CString -> Ptr DosQVariant -> IO ()