{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Graphics.UI.Qml.LowLevel.QVariant where

-- import Foreign.Ptr
import Foreign.ForeignPtr

import qualified Graphics.UI.Qml.Internal.QVariant as Raw
import qualified Graphics.UI.Qml.Internal.Types as Raw

type QVariant = ForeignPtr Raw.DosQVariant

class IsQVariant a where
    fromQVariant :: QVariant -> IO a
    toQVarient :: a -> IO QVariant

instance Integral a => IsQVariant a where
    fromQVariant var = withForeignPtr var
        (fmap fromIntegral . Raw.toInt)

    toQVarient var = do
        ptr <- Raw.createInt $ fromIntegral var
        newForeignPtr Raw.finalizer ptr