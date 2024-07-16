
module Graphics.UI.Qml.Internal.QMetaObject where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qmetaobject_create"
    create :: Ptr DosQMetaObject -> CString -> Ptr SignalDefinitions -> Ptr SlotDefinitions -> Ptr PropertyDefinitions -> IO (Ptr DosQMetaObject)

foreign import ccall "dos_qmetaobject_delete"
    delete :: Ptr DosQMetaObject -> IO ()

foreign import ccall "&dos_qmetaobject_delete"
    finalizer :: FunPtr (Ptr DosQMetaObject -> IO ())

foreign import ccall "dos_qmetaobject_invoke_method"
    invokeMethod :: Ptr DosQObject -> FunPtr (Ptr DosQObject -> Ptr () -> IO ()) -> Ptr () -> DosQtConnectionType -> IO CBool