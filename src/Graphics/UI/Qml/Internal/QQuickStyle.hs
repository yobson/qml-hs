
module Graphics.UI.Qml.Internal.QQuickStyle where

import Foreign.C

foreign import ccall "dos_qquickstyle_set_style"
    setStyle :: CString -> IO ()

foreign import ccall "dos_qquickstyle_set_fallback_style"
    setFallbackStyle :: CString -> IO ()