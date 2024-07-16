
module Graphics.UI.Qml.Internal.QApplication where

foreign import ccall "dos_qapplication_create"
    create :: IO ()

foreign import ccall "dos_qapplication_delete"
    delete :: IO ()

foreign import ccall "dos_qapplication_exec"
    exec :: IO ()

foreign import ccall "dos_qapplication_quit"
    quit :: IO ()