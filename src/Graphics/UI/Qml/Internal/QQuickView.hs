
module Graphics.UI.Qml.Internal.QQuickView where

import Foreign.Ptr
import Foreign.C
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qquickview_create"
    create :: IO (Ptr DosQQuickView)

foreign import ccall "dos_qquickview_show"
    show :: Ptr DosQQuickView -> IO ()

foreign import ccall "dos_qquickview_source"
    source :: Ptr DosQQuickView -> IO CString

foreign import ccall "dos_qquickview_set_source_url"
    setSourceUrl :: Ptr DosQQuickView -> Ptr DosQUrl -> IO ()

foreign import ccall "dos_qquickview_set_source"
    setSource :: Ptr DosQQuickView -> CString -> IO ()

foreign import ccall "dos_qquickview_set_resize_mode"
    setResizeMode :: Ptr DosQQuickView -> CInt -> IO ()

foreign import ccall "dos_qquickview_delete"
    delete :: Ptr DosQQuickView -> IO ()

foreign import ccall "&dos_qquickview_delete"
    finalizer :: FunPtr (Ptr DosQQuickView -> IO ())

foreign import ccall "dos_qquickview_rootContext"
    rootContext :: Ptr DosQQuickView -> IO (Ptr DosQQmlContext)