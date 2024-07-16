
module Graphics.UI.Qml.Internal.QPixmap where

import Foreign.Ptr
import Foreign.C
import Graphics.UI.Qml.Internal.Types

foreign import ccall "dos_qpixmap_create"
    create :: IO (Ptr DosPixmap)

foreign import ccall "dos_qpixmap_create_qpixmap"
    createQPixmap :: Ptr DosPixmap -> IO (Ptr DosPixmap)

foreign import ccall "dos_qpixmap_create_width_and_height"
    createWidthHeight :: CInt -> CInt -> IO (Ptr DosPixmap)

foreign import ccall "dos_qpixmap_delete"
    delete :: Ptr DosPixmap -> IO ()

foreign import ccall "&dos_qpixmap_delete"
    finalize :: FunPtr (Ptr DosPixmap -> IO ())

foreign import ccall "dos_qpixmap_load"
    load :: Ptr DosPixmap -> CString -> CString -> IO ()

foreign import ccall "dos_qpixmap_loadFromData"
    loadFromData :: Ptr DosPixmap -> CString -> CUInt -> IO ()

foreign import ccall "dos_qpixmap_fill"
    fill :: Ptr DosPixmap -> CUChar -> CUChar -> CUChar -> CUChar -> IO ()

foreign import ccall "dos_qpixmap_assign"
    assign :: Ptr DosPixmap -> Ptr DosPixmap -> IO ()

foreign import ccall "dos_qpixmap_isNull"
    isNull :: Ptr DosPixmap -> IO CBool