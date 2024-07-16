
module Graphics.UI.Qml.Internal.QAbstractTableModel where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qabstracttablemodel_qmetaobject"
    qMetaObject :: IO (Ptr DosQMetaObject)


foreign import ccall "dos_qabstracttablemodel_create"
    create :: Ptr () -> Ptr DosQMetaObject -> DObjectCallback -> Ptr DosQAbstractItemModelCallbacks -> IO (Ptr DosQAbstractTableModel)

foreign import ccall "dos_qabstracttablemodel_index"
    index :: Ptr DosQAbstractTableModel -> CInt -> CInt -> Ptr DosQModelIndex -> IO (Ptr DosQModelIndex)

foreign import ccall "dos_qabstracttablemodel_parent"
    parent :: Ptr DosQAbstractTableModel -> Ptr DosQModelIndex -> IO (Ptr DosQModelIndex)