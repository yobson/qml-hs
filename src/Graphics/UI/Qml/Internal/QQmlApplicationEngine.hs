
module Graphics.UI.Qml.Internal.QQmlApplicationEngine where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qqmlapplicationengine_create"
    create :: IO (Ptr DosQQmlApplicationEngine)

foreign import ccall "dos_qqmlapplicationengine_load"
    load :: Ptr DosQQmlApplicationEngine -> CString -> IO ()

foreign import ccall "dos_qqmlapplicationengine_load_url"
    loadUrl :: Ptr DosQQmlApplicationEngine -> Ptr DosQUrl -> IO ()

foreign import ccall "dos_qqmlapplicationengine_load_data"
    loadData :: Ptr DosQQmlApplicationEngine -> CString -> IO ()

foreign import ccall "dos_qqmlapplicationengine_add_import_path"
    addImportPath :: Ptr DosQQmlApplicationEngine -> CString -> IO ()

foreign import ccall "dos_qqmlapplicationengine_context"
    context :: Ptr DosQQmlApplicationEngine -> IO (Ptr DosQQmlContext)

foreign import ccall "dos_qqmlapplicationengine_addImageProvider"
    addImageProvider :: Ptr DosQQmlApplicationEngine -> CString -> Ptr DosQQuickImageProvider -> IO ()

foreign import ccall "dos_qqmlapplicationengine_delete"
    delete :: Ptr DosQQmlApplicationEngine -> IO ()

foreign import ccall "&dos_qqmlapplicationengine_delete"
    finialiser :: FunPtr (Ptr DosQQmlApplicationEngine -> IO ())