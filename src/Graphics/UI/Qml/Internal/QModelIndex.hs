
module Graphics.UI.Qml.Internal.QmodelIndex where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qmodelindex_create"
    create :: IO (Ptr DosQModelIndex)

foreign import ccall "dos_qmodelindex_create_qmodelindex"
    createQModelIndex :: Ptr DosQModelIndex -> IO (Ptr DosQModelIndex)

foreign import ccall "dos_qmodelindex_delete"
    delete :: Ptr DosQModelIndex -> IO ()

foreign import ccall "&dos_qmodelindex_delete"
    finalizer :: FunPtr (Ptr DosQModelIndex -> IO ())

foreign import ccall "dos_qmodelindex_row"
    row :: Ptr DosQModelIndex -> IO CInt

foreign import ccall "dos_qmodelindex_column"
    column :: Ptr DosQModelIndex -> IO CInt

foreign import ccall "dos_qmodelindex_isValid"
    isValid :: Ptr DosQModelIndex -> IO CBool

foreign import ccall "dos_qmodelindex_data"
    data' :: Ptr DosQModelIndex -> CInt -> IO (Ptr DosQVariant)

foreign import ccall "dos_qmodelindex_parent"
    parent :: Ptr DosQModelIndex -> IO (Ptr DosQModelIndex)

foreign import ccall "dos_qmodelindex_child"
    child :: Ptr DosQModelIndex -> CInt -> CInt -> IO (Ptr DosQModelIndex)

foreign import ccall "dos_qmodelindex_sibling"
    sibling :: Ptr DosQModelIndex -> CInt -> CInt -> IO (Ptr DosQModelIndex)

foreign import ccall "dos_qmodelindex_assign"
    assign :: Ptr DosQModelIndex -> Ptr DosQModelIndex -> IO ()

foreign import ccall "dos_qmodelindex_internalPointer"
    internalPointer :: Ptr DosQModelIndex -> IO (Ptr ())