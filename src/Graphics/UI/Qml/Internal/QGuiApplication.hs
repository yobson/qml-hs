
module Graphics.UI.Qml.Internal.QGuiApplication where

import Foreign.C

foreign import ccall "dos_qcoreapplication_application_dir_path"
    applicationDirPath :: IO CString

foreign import ccall "dos_qguiapplication_create"
    create :: IO ()

foreign import ccall "dos_qguiapplication_delete"
    delete :: IO ()

foreign import ccall "dos_qguiapplication_exec"
    exec :: IO ()

foreign import ccall "dos_qguiapplication_quit"
    quit :: IO ()