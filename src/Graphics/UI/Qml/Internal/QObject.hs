
module Graphics.UI.Qml.Internal.QObject where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qobject_qmetaobject"
    qMetaObject :: IO (Ptr DosQMetaObject)

foreign import ccall "dos_qobject_create"
    create :: Ptr () -> Ptr DosQMetaObject -> DObjectCallback -> IO (Ptr DosQObject)

foreign import ccall  "dos_qobject_signal_emit"
    signalEmit :: Ptr DosQObject -> CString -> CInt -> Ptr (Ptr ()) -> IO ()

foreign import ccall "dos_qobject_objectName"
    objectName :: Ptr DosQObject -> IO CString

foreign import ccall "dos_qobject_setObjectName"
    setObjectName :: Ptr DosQObject -> CString -> IO ()

foreign import ccall "dos_qobject_delete"
    delete :: Ptr DosQObject -> IO ()

foreign import ccall "&dos_qobject_delete"
    finalizer :: FunPtr (Ptr DosQObject -> IO ())

foreign import ccall "dos_qobject_deleteLater"
    deleteLater :: Ptr DosQObject -> IO ()

foreign import ccall "dos_qobject_property"
    property :: Ptr DosQObject -> CString -> IO (Ptr DosQVariant)

foreign import ccall "dos_qobject_setProperty"
    setProperty :: Ptr DosQObject -> CString -> Ptr DosQVariant -> IO CBool

foreign import ccall "dos_slot_macro"
    slotMacro :: CString -> IO CString

foreign import ccall "dos_signal_macro"
    signalMacro :: CString -> IO CString

foreign import ccall "dos_qobject_connect_static"
    connectStatic :: Ptr DosQObject -> CString -> Ptr DosQObject -> CString -> DosQtConnectionType -> IO ()

foreign import ccall "dos_qobject_disconnect_static"
    disconnectStatic :: Ptr DosQObject -> CString -> Ptr DosQObject -> CString -> IO ()