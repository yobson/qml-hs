
module Graphics.UI.Qml.Internal.QDeclarative where

import Foreign.C
import Foreign.Ptr
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qdeclarative_qmlregistertype"
    qmlRegisterType :: Ptr QmlRegisterType -> IO CInt

foreign import ccall "dos_qdeclarative_qmlregistersingletontype"
    registerSingletonType :: Ptr QmlRegisterType -> IO CInt