
module Graphics.UI.Qml.Internal.QUrl where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qurl_create"
    create :: CString -> CInt -> IO (Ptr DosQUrl)

foreign import ccall "dos_qurl_delete"
    delete :: Ptr DosQUrl -> IO ()

foreign import ccall "&dos_qurl_delete"
    finalizer :: FunPtr (Ptr DosQUrl -> IO ())

foreign import ccall "dos_qurl_to_string"
    urlToString :: Ptr DosQUrl -> IO CString

foreign import ccall "dos_qurl_isValid"
    isValid :: Ptr DosQUrl -> IO CBool