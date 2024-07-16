
module Graphics.UI.Qml.Internal.QAbstractTableModel where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qabstracttablemodel_qmetaobject"
    qMetaObject :: IO (Ptr DosQMetaObject)