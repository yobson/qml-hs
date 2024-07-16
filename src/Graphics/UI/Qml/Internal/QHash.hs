
module Graphics.UI.Qml.Internal.QHash where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qhash_int_qbytearray_create"
    create :: IO (Ptr DosQHashIntQByteArray)

foreign import ccall "dos_qhash_int_qbytearray_delete"
    delete :: Ptr DosQHashIntQByteArray -> IO ()

foreign import ccall "&dos_qhash_int_qbytearray_delete"
    finialiser :: FunPtr (Ptr DosQHashIntQByteArray -> IO ())

foreign import ccall "dos_qhash_int_qbytearray_insert"
    insert :: Ptr DosQHashIntQByteArray -> CInt -> CString -> IO ()

foreign import ccall "dos_qhash_int_qbytearray_value"
    value :: Ptr DosQHashIntQByteArray -> CInt -> IO CString