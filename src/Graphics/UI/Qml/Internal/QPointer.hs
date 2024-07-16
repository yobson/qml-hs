
module Graphics.UI.Qml.Internal.QPointer where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qpointer_create"
    create :: Ptr DosQObject -> IO (Ptr DosQPointer)

foreign import ccall "dos_qpointer_delete"
    delete :: Ptr DosQPointer -> IO ()

foreign import ccall "&dos_qpointer_delete"
    finalizer :: FunPtr (Ptr DosQPointer -> IO ())

foreign import ccall "dos_qpointer_is_null"
    isNull :: Ptr DosQPointer -> IO CBool

foreign import ccall "dos_qpointer_clear"
    clear :: Ptr DosQPointer -> IO ()

foreign import ccall "dos_qpointer_data"
    data' :: Ptr DosQPointer -> IO (Ptr DosQObject)