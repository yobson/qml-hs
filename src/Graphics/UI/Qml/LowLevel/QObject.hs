{-# LANGUAGE StrictData, RankNTypes #-}

module Graphics.UI.Qml.LowLevel.QObject where

import qualified Graphics.UI.Qml.Internal.QObject as Raw
import qualified Graphics.UI.Qml.Internal.Types as Raw
import qualified Graphics.UI.Qml.Internal.QVariant as QV
import qualified Data.Map as Map

import Graphics.UI.Qml.LowLevel.QMetaObject
import Graphics.UI.Qml.LowLevel.QVariant
import Control.Concurrent.STM

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import qualified Foreign.Concurrent as F

data QObject = QObject
  { metaObj :: QMetaObject
  , rawObj  :: ForeignPtr Raw.DosQObject
  }

objectCallback :: CallBackMap -> IO Raw.DObjectCallback
objectCallback cbm = mkCallBack $ \_ slotName args vars -> do
  sltNameFPtr <- newForeignPtr_ slotName
  sltnm <- fromQVariant sltNameFPtr
  vfargs <- peekArray (fromIntegral args) vars
  vargs  <- mapM newForeignPtr_ vfargs
  case Map.lookup sltnm cbm of
    Just chan -> do
      atomically $ writeTChan chan vargs
      return ()
    Nothing -> return ()
  return ()

foreign import ccall "wrapper" mkCallBack 
  :: (Ptr () -> Ptr Raw.DosQVariant -> CInt -> Ptr (Ptr Raw.DosQVariant) -> IO ())
  -> IO Raw.DObjectCallback

objToVariant :: QObject -> IO QVariant
objToVariant (QObject mo ro) = do
  ptr <- QV.create
  withForeignPtr ro $ \optr ->
    QV.setQObject ptr optr
  F.newForeignPtr ptr $ do
    touchForeignPtr ro
    touchForeignPtr $ qMetaObjectPtr mo
    QV.delete ptr


newQObject :: String -> QMetaObject -> IO QObject
newQObject name qmo@(QMetaObject mo cbm) = withForeignPtr mo $ \o -> do
  callback <- objectCallback cbm
  className <- newCString name
  ptr <- Raw.create nullPtr o callback
  Raw.setObjectName ptr className
  fptr <- newForeignPtr Raw.finalizer ptr
  return $ QObject qmo fptr
