
module Graphics.UI.Qml.Internal.QAbstractItemModel where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qabstractitemmodel_qmetaobject"
    qMetaObject :: IO (Ptr DosQMetaObject)

foreign import ccall "dos_qabstractitemmodel_create"
    create :: Ptr () -> Ptr DosQMetaObject -> DObjectCallback -> Ptr DosQAbstractItemModelCallbacks -> IO (Ptr DosQAbstractItemModel)

foreign import ccall "dos_qabstractitemmodel_setData"
    setData :: Ptr DosQAbstractItemModel -> Ptr DosQModelIndex -> Ptr DosQVariant -> CInt -> IO CBool

foreign import ccall "dos_qabstractitemmodel_roleNames"
    roleNames :: Ptr DosQAbstractItemModel -> IO (Ptr DosQHashIntQByteArray)

foreign import ccall "dos_qabstractitemmodel_flags"
    flags :: Ptr DosQAbstractItemModel -> Ptr DosQModelIndex -> IO CInt

foreign import ccall "dos_qabstractitemmodel_headerData"
    headerData :: Ptr DosQAbstractItemModel -> CInt -> CInt -> CInt -> IO (Ptr DosQVariant)

foreign import ccall "dos_qabstractitemmodel_hasChildren"
    hasChildren :: Ptr DosQAbstractItemModel -> Ptr DosQModelIndex -> IO CBool

foreign import ccall "dos_qabstractitemmodel_hasIndex"
    hasIndex :: Ptr DosQAbstractItemModel -> CInt -> CInt -> Ptr DosQModelIndex -> IO CBool

foreign import ccall "dos_qabstractitemmodel_canFetchMore"
    canFetchMore :: Ptr DosQAbstractItemModel -> Ptr DosQModelIndex -> IO CBool

foreign import ccall "dos_qabstractitemmodel_fetchMore"
    fetchMore :: Ptr DosQAbstractItemModel -> Ptr DosQModelIndex -> IO ()

foreign import ccall "dos_qabstractitemmodel_beginInsertRows"
    beginInsertRows :: Ptr DosQAbstractItemModel -> Ptr DosQModelIndex -> CInt -> CInt -> IO ()

foreign import ccall "dos_qabstractitemmodel_endInsertRows"
    endInsertRows :: Ptr DosQAbstractItemModel -> IO ()

foreign import ccall "dos_qabstractitemmodel_beginRemoveRows"
    beginRemoveRows :: Ptr DosQAbstractItemModel -> Ptr DosQModelIndex -> CInt -> CInt -> IO ()

foreign import ccall "dos_qabstractitemmodel_endRemoveRows"
    endRemoveRows :: Ptr DosQAbstractItemModel -> IO ()

foreign import ccall "dos_qabstractitemmodel_beginInsertColumns"
    beginInsertColumns :: Ptr DosQAbstractItemModel -> Ptr DosQModelIndex -> CInt -> CInt -> IO ()

foreign import ccall "dos_qabstractitemmodel_endInsertColumns"
    endInsertColumns :: Ptr DosQAbstractItemModel -> IO ()

foreign import ccall "dos_qabstractitemmodel_beginRemoveColumns"
    beginRemoveColumns :: Ptr DosQAbstractItemModel -> Ptr DosQModelIndex -> CInt -> CInt -> IO ()

foreign import ccall "dos_qabstractitemmodel_endRemoveColumns"
    endRemoveColumns :: Ptr DosQAbstractItemModel -> IO ()

foreign import ccall "dos_qabstractitemmodel_beginResetModel"
    beginResetModel :: Ptr DosQAbstractItemModel -> IO ()

foreign import ccall "dos_qabstractitemmodel_endResetModel"
    endResetModel :: Ptr DosQAbstractItemModel -> IO ()

foreign import ccall "dos_qabstractitemmodel_dataChanged"
    dataChanged :: Ptr DosQAbstractItemModel -> Ptr DosQModelIndex -> Ptr DosQModelIndex -> Ptr CInt -> CInt -> IO ()

foreign import ccall "dos_qabstractitemmodel_createIndex"
    createIndex :: Ptr DosQAbstractItemModel -> CInt -> CInt -> Ptr () -> IO (Ptr DosQModelIndex)