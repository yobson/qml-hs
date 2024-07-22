{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.UI.Qml.LowLevel.QVariant where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Data.Proxy
import Control.Monad

import qualified Graphics.UI.Qml.Internal.QVariant as Raw
import qualified Graphics.UI.Qml.Internal.String as DS
import qualified Graphics.UI.Qml.Internal.Types as Raw

type QVariant = ForeignPtr Raw.DosQVariant

type family (IsList a) :: Bool where
  IsList Char  = 'False
  IsList Int     = 'True
  IsList Bool    = 'True
  IsList CString = 'True
  IsList QVariant = 'True

class IsQVariant a where
    fromQVariant :: QVariant -> IO a
    unsafeToQVariant :: a -> IO (Ptr Raw.DosQVariant)
    toQVariant :: a -> IO QVariant
    toQVariant x = unsafeToQVariant x >>= newForeignPtr Raw.finalizer
    setQVarient :: QVariant -> a -> IO ()
    metaType :: a -> CInt

instance (IsList a ~ flag, IsQVariant' a flag) => IsQVariant [a] where
  fromQVariant = fromQVariant' (Proxy :: Proxy flag)
  unsafeToQVariant = unsafeToQVariant' (Proxy :: Proxy flag)
  toQVariant = toQVariant' (Proxy :: Proxy flag)
  setQVarient = setQVarient' (Proxy :: Proxy flag)
  metaType = metaType' (Proxy :: Proxy flag)

class IsQVariant' a (flag :: Bool) where
    fromQVariant' :: Proxy flag -> QVariant -> IO [a]
    unsafeToQVariant' :: Proxy flag -> [a] -> IO (Ptr Raw.DosQVariant)
    toQVariant' :: Proxy flag -> [a] -> IO QVariant
    toQVariant' p x = unsafeToQVariant' p x >>= newForeignPtr Raw.finalizer
    setQVarient' :: Proxy flag -> QVariant -> [a] -> IO ()
    metaType' :: Proxy flag -> [a] -> CInt

instance IsQVariant CString where
    fromQVariant var = withForeignPtr var $ \ptr -> do
      Raw.toString ptr

    unsafeToQVariant = Raw.createString

    setQVarient var str = withForeignPtr var $ \ptr ->
        Raw.setString ptr str
    metaType _ = 10

instance IsQVariant' Char 'False where
    fromQVariant' _ var = do
      cstr <- fromQVariant var
      str <- peekCString cstr
      DS.delete cstr
      return str

    unsafeToQVariant' _ str = withCString str unsafeToQVariant
    setQVarient' _ var str = withCString str $ \cstr ->
        setQVarient var cstr
    metaType' _ _ = 10

instance IsQVariant Int where
    fromQVariant var = fromIntegral <$> withForeignPtr var Raw.toInt

    unsafeToQVariant = Raw.createInt . fromIntegral

    setQVarient var cint = withForeignPtr var $ \ptr ->
        Raw.setInt ptr $ fromIntegral cint
    metaType _ = 2

instance IsQVariant CBool where
    fromQVariant var = withForeignPtr var Raw.toBool

    unsafeToQVariant = Raw.createBool

    setQVarient var b = withForeignPtr var $ \ptr ->
        Raw.setBool ptr b
    metaType _ = 1

instance IsQVariant QVariant where
  fromQVariant = return

  toQVariant = return 
  unsafeToQVariant _ = error "Will break garbage collector"

  setQVarient var b = withForeignPtr var $ \ptr ->
    withForeignPtr b $ \other ->
      Raw.assign ptr other
  metaType _ = 41

instance (IsQVariant a) => IsQVariant' a 'True where
  metaType' _ _ = 9

  fromQVariant' _ var = withForeignPtr var $ \ptr -> do
    arr <- Raw.toArray ptr
    vararr <- peek arr
    out <- peekArray (fromIntegral $ Raw.dqvaSize vararr) (Raw.dqvaData vararr)
    Raw.deleteArray arr
    forM out $ \qptr -> do
      qvar <- newForeignPtr_ qptr
      fromQVariant qvar

  unsafeToQVariant' _ x = do
    varX <- mapM unsafeToQVariant x
    buff <- mallocForeignPtrArray (length x)
    withForeignPtr buff $ \ubuff -> do
      pokeArray ubuff varX
      Raw.createArray (fromIntegral $ length x) ubuff

  setQVarient' _ var x = withForeignPtr var $ \ptr -> do
    varX <- mapM unsafeToQVariant x
    buff <- mallocForeignPtrArray (length x)
    withForeignPtr buff $ \ubuff -> do
      pokeArray ubuff varX
      Raw.setArray ptr ubuff

sameVar :: forall a b . (Eq a, IsQVariant a, IsQVariant b) => a -> b -> IO Bool
sameVar x y = do
  let mx = metaType x
      my = metaType y
  if mx /= my 
     then return False
     else do
              (sx :: a) <- toQVariant x >>= fromQVariant
              (sy :: a) <- toQVariant y >>= fromQVariant
              return (sx == sy)

bool2cbool :: Bool -> CBool
bool2cbool True  = 1
bool2cbool False = 0

cbool2bool :: CBool -> Bool
cbool2bool 0 = False
cbool2bool _ = True

instance IsQVariant Bool  where
    fromQVariant var = cbool2bool <$> fromQVariant var

    unsafeToQVariant = unsafeToQVariant . bool2cbool

    setQVarient var = setQVarient var . bool2cbool
    metaType _ = 1


-- TODO How to do arrays? As QVariant or QVarient Array?
-- TODO QObject
