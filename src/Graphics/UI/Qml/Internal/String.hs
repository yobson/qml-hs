
module Graphics.UI.Qml.Internal.String where

import Foreign.C
import Foreign.Ptr

foreign import ccall "dos_chararray_delete"
    delete :: CString -> IO ()

foreign import ccall "&dos_chararray_delete"
    finalizer :: FunPtr (CString -> IO ())