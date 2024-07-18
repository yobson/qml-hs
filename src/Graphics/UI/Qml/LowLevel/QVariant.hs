{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module Graphics.UI.Qml.LowLevel.QVariant where

import Foreign.C
import Foreign.ForeignPtr

import qualified Graphics.UI.Qml.Internal.QVariant as Raw
import qualified Graphics.UI.Qml.Internal.String as DS
import qualified Graphics.UI.Qml.Internal.Types as Raw

type QVariant = ForeignPtr Raw.DosQVariant

class IsQVariant a where
    fromQVariant :: QVariant -> IO a
    toQVarient :: a -> IO QVariant
    setQVarient :: QVariant -> a -> IO ()
    metaType :: a -> CInt

instance IsQVariant CString where
    fromQVariant var = withForeignPtr var $ \ptr -> do
      Raw.toString ptr

    toQVarient str = do
        ptr <- Raw.createString str
        newForeignPtr Raw.finalizer ptr

    setQVarient var str = withForeignPtr var $ \ptr ->
        Raw.setString ptr str
    metaType _ = 10

instance IsQVariant String where
    fromQVariant var = do
      cstr <- fromQVariant var
      str <- peekCString cstr
      DS.delete cstr
      return str

    toQVarient str = withCString str toQVarient
    setQVarient var str = withCString str $ \cstr ->
        setQVarient var cstr
    metaType _ = 10

instance IsQVariant Int where
    fromQVariant var = fromIntegral <$> withForeignPtr var Raw.toInt

    toQVarient cint = do
        ptr <- Raw.createInt $ fromIntegral cint
        newForeignPtr Raw.finalizer ptr

    setQVarient var cint = withForeignPtr var $ \ptr ->
        Raw.setInt ptr $ fromIntegral cint
    metaType _ = 2

instance IsQVariant CBool where
    fromQVariant var = withForeignPtr var Raw.toBool

    toQVarient b = do
        ptr <- Raw.createBool b
        newForeignPtr Raw.finalizer ptr

    setQVarient var b = withForeignPtr var $ \ptr ->
        Raw.setBool ptr b
    metaType _ = 1

instance IsQVariant QVariant where
  fromQVariant = return

  toQVarient = return 

  setQVarient var b = withForeignPtr var $ \ptr ->
    withForeignPtr b $ \other ->
      Raw.assign ptr other
  metaType _ = 41

sameVar :: (IsQVariant a, IsQVariant b) => a -> b -> IO Bool
sameVar x y = do
  let mx = metaType x
      my = metaType y
  if mx /= my 
     then return False
     else case mx of
            2 -> do
              (sx :: Int) <- toQVarient x >>= fromQVariant
              (sy :: Int) <- toQVarient y >>= fromQVariant
              return (sx == sy)
            1 -> do
              (sx :: Bool) <- toQVarient x >>= fromQVariant
              (sy :: Bool) <- toQVarient y >>= fromQVariant
              return (sx == sy)
            _ -> do
              (sx :: String) <- toQVarient x >>= fromQVariant
              (sy :: String) <- toQVarient y >>= fromQVariant
              return (sx == sy)

bool2cbool :: Bool -> CBool
bool2cbool True  = 1
bool2cbool False = 0

cbool2bool :: CBool -> Bool
cbool2bool 0 = False
cbool2bool _ = True

instance IsQVariant Bool where
    fromQVariant var = cbool2bool <$> fromQVariant var

    toQVarient = toQVarient . bool2cbool

    setQVarient var = setQVarient var . bool2cbool
    metaType _ = 1


-- TODO How to do arrays? As QVariant or QVarient Array?
-- TODO QObject
