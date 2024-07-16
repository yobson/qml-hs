
module Graphics.UI.Qml.Internal.QVariant where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qvariantarray_delete"
    deleteArray :: Ptr DosQVariantArray -> IO ()

foreign import ccall "&dos_qvariantarray_delete"
    arrayFinalizer :: FunPtr (Ptr DosQVariantArray -> IO ())

foreign import ccall "dos_qvariant_create"
    create :: IO (Ptr DosQVariant)

foreign import ccall "dos_qvariant_create_int"
    createInt :: CInt -> IO (Ptr DosQVariant)

foreign import ccall "dos_qvariant_create_bool"
    createBool :: CBool -> IO (Ptr DosQVariant)

foreign import ccall "dos_qvariant_create_string"
    createString :: CString -> IO (Ptr DosQVariant)

foreign import ccall "dos_qvariant_create_qobject"
    createQObject :: Ptr DosQObject -> IO (Ptr DosQVariant)

foreign import ccall "dos_qvariant_create_qvariant"
    createQVariant :: Ptr DosQVariant -> IO (Ptr DosQVariant)

foreign import ccall "dos_qvariant_create_float"
    createFloat :: CFloat -> IO (Ptr DosQVariant)

foreign import ccall "dos_qvariant_create_double"
    createDouble :: CDouble -> IO (Ptr DosQVariant)

foreign import ccall "dos_qvariant_create_array"
    createArray :: CInt -> Ptr (Ptr DosQVariant) -> IO (Ptr DosQVariant)

foreign import ccall "dos_qvariant_setInt"
    setInt :: Ptr DosQVariant -> CInt -> IO ()

foreign import ccall "dos_qvariant_setBool"
    setBool :: Ptr DosQVariant -> CBool -> IO ()

foreign import ccall "dos_qvariant_setFloat"
    setFloat :: Ptr DosQVariant -> CFloat -> IO ()

foreign import ccall "dos_qvariant_setDouble"
    setDouble :: Ptr DosQVariant -> CDouble -> IO ()

foreign import ccall "dos_qvariant_setString"
    setString :: Ptr DosQVariant -> CString -> IO ()

foreign import ccall "dos_qvariant_setQObject"
    setQObject :: Ptr DosQVariant -> Ptr DosQObject -> IO ()

foreign import ccall "dos_qvariant_setArray"
    setArray :: Ptr DosQVariant -> Ptr (Ptr DosQVariant) -> IO ()

foreign import ccall "dos_qvariant_isnull"
    isNull :: Ptr DosQVariant -> IO CBool

foreign import ccall "dos_qvariant_delete"
    delete :: Ptr DosQVariant -> IO ()

foreign import ccall "&dos_qvariant_delete"
    finalizer :: FunPtr (Ptr DosQVariant -> IO ())

foreign import ccall "dos_qvariant_assign"
    assign :: Ptr DosQVariant -> Ptr DosQVariant -> IO ()

foreign import ccall "dos_qvariant_toInt"
    toInt :: Ptr DosQVariant -> IO CInt

foreign import ccall "dos_qvariant_toBool"
    toBool :: Ptr DosQVariant -> IO CBool

foreign import ccall "dos_qvariant_toString"
    toString :: Ptr DosQVariant -> IO CString

foreign import ccall "dos_qvariant_toFloat"
    toFloat :: Ptr DosQVariant -> IO CFloat

foreign import ccall "dos_qvariant_toDouble"
    toDouble :: Ptr DosQVariant -> IO CDouble

foreign import ccall "dos_qvariant_toArray"
    toArray :: Ptr DosQVariant -> IO (Ptr DosQVariantArray)

foreign import ccall "dos_qvariant_toQObject"
    toQObject :: Ptr DosQVariant -> IO (Ptr DosQObject)