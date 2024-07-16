
module Graphics.UI.Qml.Internal.QQuickImageProvider where

import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qquickimageprovider_create"
    create :: RequestPixmapCallback -> IO (Ptr DosQQuickImageProvider)

foreign import ccall "dos_qquickimageprovider_delete"
    delete :: Ptr DosQQuickImageProvider -> IO ()

foreign import ccall "&dos_qquickimageprovider_delete"
    finialiser :: FunPtr (Ptr DosQQuickImageProvider -> IO ())