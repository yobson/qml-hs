
module Graphics.UI.Qml.Internal.QAbstractListModel where

import Foreign.C.Types
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qabstractlistmodel_qmetaobject"
    qMetaObject :: IO (Ptr DosQMetaObject)

foreign import ccall "dos_qabstractlistmodel_create"
    create :: Ptr () -> Ptr DosQMetaObject -> DObjectCallback -> Ptr DosQAbstractItemModelCallbacks -> IO (Ptr DosQAbstractListModel)

foreign import ccall "dos_qabstractlistmodel_index"
    index :: Ptr DosQAbstractListModel -> CInt -> CInt -> Ptr DosQModelIndex -> IO (Ptr DosQModelIndex)

foreign import ccall "dos_qabstractlistmodel_parent"
    parent :: Ptr DosQAbstractListModel -> Ptr DosQModelIndex -> IO (Ptr DosQModelIndex)

foreign import ccall "dos_qabstractlistmodel_columnCount"
    columnCount :: Ptr DosQAbstractListModel -> Ptr DosQModelIndex -> IO CInt